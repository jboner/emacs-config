/*
 * $Id: ClassPathEntry.java,v 1.22 2006/04/07 22:47:38 surajacharya Exp $
 * Copyright (C) 2004 Suraj Acharya (sacharya@cs.indiana.edu)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

package jde.util.usages;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import jde.util.Usages;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ConstantPoolVisitor;
import java.util.Collections;
import java.util.zip.GZIPOutputStream;
import java.util.zip.GZIPInputStream;
import java.io.BufferedOutputStream;

public abstract class ClassPathEntry implements Serializable {

    ObjectToCollectionMap _classdeps;
    Set _classes;
    ObjectToCollectionMap _subs = new ObjectToCollectionMap(TreeSet.class);
    long _lastModified;
    File _path;
    String _stringPath;

    transient Map _classNameMap;
    transient LinkedList _classpaths = new LinkedList();
    transient LinkedList _orders = new LinkedList();

    transient static Map _allEntries = new HashMap();
    
    public String toString () {
        return getStringPath();
    }

    public Collection getChildren() {
        return Collections.EMPTY_LIST;
    }

    public static ClassPathEntry get (File file) {
        String path = file.getPath().intern();
        ClassPathEntry cpe = null;
        Reference ref = (Reference) _allEntries.get (path);
        if (ref == null || ref.get() == null) {
            cpe = getCachedCPE (file);
            if (cpe == null) {
                String name = file.getName();
                if (file.isFile() || name.endsWith (".ear") || name.endsWith (".jar") || name.endsWith (".war") || name.endsWith (".zip"))
                    cpe = new ClassPathEntry.Jar (file);
                else
                    cpe = new ClassPathEntry.Dir (file);
            }
            ref = new SoftReference (cpe);
            _allEntries.put (path, ref);
        } else {
            cpe = (ClassPathEntry) ref.get();
        }
        return cpe;
    }

    static ClassPathEntry getCachedCPE (File file) {
        return getCachedCPEFile (file);
    }
    
    static void cacheCPE (ClassPathEntry cpe) {
        cacheCPEFile (cpe);
    }
    
    private static ClassPathEntry getCachedCPEFile (File file) {
        
        File cacheDir = new File (System.getProperty("user.home"), ".jde-usages");
        if (cacheDir.exists() && cacheDir.isDirectory() && !System.getProperty("java.version").startsWith ("1.3")) {
            File cacheFile = new File (cacheDir, file.getPath().replaceAll("#", "##").replaceAll ("/", "#.").replaceAll ("\\\\", "#.").replaceAll (":", "#_"));
            if (cacheFile.exists() && cacheFile.isFile()) {
                ClassPathEntry cpe;
                ObjectInputStream ois = null;
                try {
                    FileInputStream fis = new FileInputStream(cacheFile);
//                  ois = new ObjectInputStream(new GZIPInputStream (new BufferedInputStream (fis)));
                    ois = new ObjectInputStream(new BufferedInputStream (fis));
                    cpe = (ClassPathEntry) ois.readObject();
                    return cpe;
                } catch (IOException e) {
                    cpe = null;
                } catch (ClassNotFoundException e) {
                    cpe = null;
                } finally {
                    if (ois != null)
                        try { ois.close(); } catch (IOException ee) {}
                }
            }
        }
        return null;
    }

    private static void cacheCPEFile (ClassPathEntry cpe) {
        // cache both jars and directories
        File cacheDir = new File (System.getProperty("user.home"), ".jde-usages");  //TODO:make user-configurable
        if (cacheDir.exists() && cacheDir.isDirectory() && !System.getProperty("java.version").startsWith ("1.3")) {
            File cacheFile = new File (cacheDir, cpe._path.getPath().replaceAll("#", "##").replaceAll ("/", "#.").replaceAll ("\\\\", "#.").replaceAll (":", "#_"));
            if (cacheFile.lastModified() >= cpe._lastModified)
                return; // skip
            Usages.message ("writing " + cpe._path.getPath() + "...");
            ObjectOutputStream oos = null;
            try {
                FileOutputStream fos = new FileOutputStream(cacheFile);
//              oos = new ObjectOutputStream(new GZIPOutputStream (fos));
                oos = new ObjectOutputStream(new BufferedOutputStream (fos));
                cpe.writeObject (oos);
                oos.close();
                cacheFile.setLastModified (cpe._lastModified);
            } catch (IOException e) {
                e.printStackTrace();
                if (oos != null)
                    try {
                        oos.close();
                    } catch (IOException ee) {}
            }
        }       
    }

    void writeObject (ObjectOutputStream oos) throws IOException {
        oos.writeObject (this);
    }
    
    public ClassPathEntry (File path) {
        _path = path;
        _stringPath = path.getPath().intern();
        _lastModified = -1;
        _classdeps = new ObjectToCollectionMap(ArrayList.class);
        _classes = new HashSet();
    }

    public void addClassPathManager (ClassPathManager classpath, int order) {
        if (_classpaths == null) {
            this._classpaths = new LinkedList();
            this._orders = new LinkedList();
        }

        _classpaths.add (classpath);
        _orders.add (new Integer (order));
    }

    public void removeClassPathManager (ClassPathManager classpath) {
        if (_classpaths != null) {
            for (Iterator i = _classpaths.iterator(), j = _orders.iterator();i.hasNext();) {
                ClassPathManager classpath2 = (ClassPathManager) i.next();
                j.next();
                if (classpath2 == classpath) {
                    i.remove();j.remove();
                    return;
                }
            }
        }

        throw new RuntimeException ("removeClassPathManager: " + classpath + " is not a classpath manager for " + this);
    }

    public void setClassPathManagers (LinkedList classpaths, LinkedList orders) {
        if (_classpaths == null || _classpaths.size() == 0) {
            this._classpaths = classpaths;
            this._orders = orders;
        } else
            throw new RuntimeException ("trying to set all classpaths for a existing classpath entry");

    }

    protected List getClassPathManagers () {
        return _classpaths;
    }

    public int getOrder (ClassPathManager classpath) {
        if (_classpaths.size() == 1)
            return ((Integer) _orders.get(0)).intValue();
        else {
            for (Iterator i = _classpaths.iterator(), j = _orders.iterator();i.hasNext();) {
                ClassPathManager classpath2 = (ClassPathManager) i.next();
                int order = ((Integer)j.next()).intValue();
                if (classpath2 == classpath)
                    return order;
            }
            return -1;
        }
    }

    
    public File getPath() {return _path;}

    public String getStringPath() { return _stringPath; }

    public abstract void refresh() throws IOException;
    
    public Collection getPossibleUsers(Object className, Collection collect) {
        Collection res = _classdeps.getC (className);;
        if (res == null)
            return collect;

        if (collect == null)
            collect = new HashSet();

        collect.addAll (res);
        return collect;
    }

    public String getClassNameString (Object className) {
        String classNameS = ClassName.getClassNameString (className);
        if (classNameS != null) {
            return classNameS;
        }
        // use the transient class name cache, building it if necessarye
        if (_classNameMap == null) {
            _classNameMap = makeClassNameMap();
        }

        return (String) _classNameMap.get (className);
        
    }
    
    public abstract byte[] getBytes (Object className);

    public abstract Map makeClassNameMap ();


    /**
     * Update subclass table in _classpath. 
     * This needs to be done even if this classpath entry is not the matching one for
     * className in _classpath._definitions, as it might be for one of the subclasses.
     *
     * @param className a <code>String</code> value
     * @param cr a <code>ClassReader</code> value
     */
    void updateSubclassTable (String className, ClassReader cr) {
        String superClass = cr.getSuperClassName();
        if (superClass != null) {
            _subs.addToCollection (ClassName.makeClassName (superClass), ClassName.makeClassName (className));
        }
        
        String [] ints = cr.getImplementedInterfaces();
        for (int i =0; i < ints.length; i++) {
            superClass = ints[i].intern();
            _subs.addToCollection (ClassName.makeClassName (superClass), ClassName.makeClassName (className));
        }
    }



    static public class Jar extends ClassPathEntry {
        public Jar (File path) {
            super (path);
        }

        public void refresh()  throws IOException {
            long start = System.currentTimeMillis();
            if (!_path.exists()) {
                if (_lastModified == -1)
                    return ;
                
                // the jar has been deleted
                for (Iterator i = _classpaths.iterator(), j = _orders.iterator();i.hasNext();) {
                    ClassPathManager classpath = (ClassPathManager) i.next();
                    int order = ((Integer)j.next()).intValue();
                    classpath.findNewLocations (_classes,order);
                }
                // Collection classes = _classpath.getClasses (this);
                _classdeps.clear();
                _classes.clear();
                _lastModified = -1;
            }

            if (_lastModified >=  _path.lastModified())
                return;
            
            _classdeps.clear();
            _classes.clear();
            try {
                ZipFile jar = new JarFile (getPath(), false);
                _lastModified = _path.lastModified();
                for (Enumeration e = jar.entries();e.hasMoreElements();) { 
                    ZipEntry ze = (ZipEntry) e.nextElement();
                    if (!ze.getName().endsWith (".class"))
                        continue;

                    try {
                        ClassReader cr = new ClassReader(ClassPathManager.loadFile (jar, ze));
                        final String classNameS = cr.getClassName().intern();
                        final Object className = ClassName.makeClassName (classNameS);
                        _classes.add (className);

                        updateSubclassTable (classNameS, cr);

                        cr.accept (new ConstantPoolVisitor () {
                                public void visitClass (String clazz) {
                                    _classdeps.addToCollection (ClassName.makeClassName (clazz), className);
                                }
                            });
                    } catch (Exception ex) { Usages.message (ex + " " + _path + " " + ze.getName()); }

                        
                }

                jar.close();
            } catch (IOException e)   { Usages.message (e + " " + _path);}

            for (Iterator i = _classpaths.iterator(), j = _orders.iterator();i.hasNext();) {
                ClassPathManager classpath = (ClassPathManager) i.next();
                classpath.rebuildSubsAndDeps (this);
            }
                        
            
            
            long stop = System.currentTimeMillis();
//             if (Usages.DEBUG)
//                 Usages.message (_path + " " + (stop - start));

        }

        public byte[] getBytes (Object className) {
            String classNameS = getClassNameString (className);
            if (classNameS == null){
                Usages.message ("null classNameS for " + className + " in cpe for " + getPath());
                return null;
            }
            ZipFile zf = null;
            try {
                if (zf == null)
                    zf = new ZipFile (this.getPath());
                byte bytes[] = ClassPathManager.loadFile (zf, classNameS + ".class");
                if (zf != null)
                    zf.close();
                return bytes;

            } catch (IOException e) {
                return null;
            }
        }

        public Map makeClassNameMap () {
            Map map = new HashMap();
            try {
                ZipFile jar = new JarFile (getPath(), false);
                for (Enumeration e = jar.entries();e.hasMoreElements();) { 
                    ZipEntry ze = (ZipEntry) e.nextElement();
                    String name = ze.getName();
                    if (!name.endsWith (".class"))
                        continue;
                    name = name.substring (0, name.length() - 6).intern(); // 6 = ".class".length
                    map.put (ClassName.makeClassName(name), name);
                }
                jar.close();
            } catch (IOException e) {
                Usages.message (e + " " + _path);// do nothing
            }
            return map;
        }

        
    }
        
    static public class Dir extends ClassPathEntry {

        File[] _children = null;
        long _childrenLastMod = -1;
        Collection _cpeChildren = new LinkedList();
        public Dir (File path) {
            super (path);
        }

        public Collection getChildren() {
            return _cpeChildren;
        }

        public Collection getPossibleUsers (Object className, Collection collect) {
            collect = super.getPossibleUsers (className, collect);
            for (Iterator i = _cpeChildren.iterator();i.hasNext();)
                collect = ((ClassPathEntry) i.next()).getPossibleUsers (className, collect);
            return collect;
        }

        public void refresh() throws IOException  {
            long start = System.currentTimeMillis();

            // refresh child directories and remove entries for deleted directories
            for (Iterator i = _cpeChildren.iterator();i.hasNext();) {
                Dir child = ((Dir) i.next());
                if (child.getPath().exists()) {
                    if (child.getClassPathManagers() == null || child.getClassPathManagers().size() == 0) 
                        child.setClassPathManagers(_classpaths, _orders);
                    child.refresh();
                } else
                    i.remove();
            }            
            

            if (!_path.exists() && _lastModified == -1)
                return ; // this dir didn't exist during the last refresh
                         // either so there's nothing to do

            Set oldClasses = null;

            // if the lastMod of the directory has changed a file or subdir has been deleted/created/renamed
            if (_children == null || _path.lastModified() > _lastModified || !_path.exists()) {
                _children = _path.listFiles ();
                oldClasses = new HashSet (_classes);
            }
            int cLength = _children != null ? _children.length  : 0;

            boolean reCalc  = false;
            long maxLastMod = _path.lastModified();

            int numClasses = 0;
            for (int i = 0; i < cLength; i++) {
                File path = _children[i];
                long childLastMod = path.lastModified();
                if (path.isFile()) {
                    numClasses++;
                    if (childLastMod > _childrenLastMod) {
                        reCalc = true; 
                        if (childLastMod > maxLastMod)
                            maxLastMod = childLastMod;
                    }
                } else if (oldClasses != null && path.isDirectory() && notMyChild (path)) {
                    // a new directory has been created, we need to init a
                    // new ClassPathEntry.Dir object
                    ClassPathEntry cpe = get (path);
                    
                    if (cpe.getClassPathManagers() == null || cpe.getClassPathManagers().size() == 0) {
                        cpe.setClassPathManagers(_classpaths, _orders);
                    } else if (cpe.getClassPathManagers() != this.getClassPathManagers()) {
                        if (Usages.DEBUG)
                            Usages.message ("Overlapping classpath entries " + cpe.getPath() + " and " + getPath() + ". Search results maybe incorrect.");
                    }

                    cpe.refresh();
                    
                    for (Iterator it = _classpaths.iterator();it.hasNext();) {
                        ClassPathManager classpath = (ClassPathManager) it.next();
                            classpath.addEntryAndRebuildSubsAndDeps (cpe);
                    }

                    _cpeChildren.add (cpe);                    
                    
                }
            }


            if (!reCalc && _lastModified != -1 && _path.exists() && numClasses == _classes.size())
                return;

            _classdeps.clear();
            _classes.clear();

            for (int i = 0; i < cLength; i++) {
                File path = _children[i];
                if (path.isFile ()) {
                    String className = path.getName ();
                    if (className.endsWith (".class")) {
                        try {
                            ClassReader cr = new ClassReader(ClassPathManager.loadFile (path));
                            className = cr.getClassName().intern();

                            updateSubclassTable (className, cr);

                            _classes.add (ClassName.makeClassName (className));

                            final String cName = className;  // need a final var for the inner class
                            cr.accept (new ConstantPoolVisitor () {
                                    public void visitClass (String clazz) {
                                        _classdeps.addToCollection (ClassName.makeClassName (clazz), ClassName.makeClassName (cName));
                                    }
                                });
                        } catch (ArrayIndexOutOfBoundsException e) {
                            Usages.message (e + " " + _path);// do nothing
                        }
                        
                    }
                }
            }


            for (Iterator i = _classpaths.iterator(), j = _orders.iterator();i.hasNext();) {
                ClassPathManager classpath = (ClassPathManager) i.next();
                    classpath.rebuildSubsAndDeps (this);
            }
            
            _childrenLastMod = maxLastMod;
            _lastModified = _path.lastModified();

            if (oldClasses != null) { // check if any classes were deleted
                oldClasses.removeAll(_classes);
                if (oldClasses.size() > 0) { //
                    for (Iterator it = _classpaths.iterator(), j = _orders.iterator();it.hasNext();) {
                        ClassPathManager classpath = (ClassPathManager) it.next();
                        int order = ((Integer)j.next()).intValue();
                        classpath.findNewLocations (oldClasses,order);
                    }
                }
            }
            long stop = System.currentTimeMillis();
//             if (Usages.DEBUG)
//                 Usages.message (_path + " " + (stop - start));

        }


        private boolean notMyChild (File child) {
            for (Iterator i = _cpeChildren.iterator();i.hasNext();)
                if (((Dir) i.next()).getPath().equals (child))
                    return false;

            return true;
        }
        

        public byte[] getBytes (Object className) {
            String classNameS = getClassNameString (className);
            if (classNameS == null){
                Usages.message ("null classNameS for " + className + " in cpe for " + getPath());
                return null;
            } else 
                return ClassPathManager.loadFile (new File (this.getPath(), classNameS.substring (classNameS.lastIndexOf ('/') + 1) + ".class"));
        }

        public Map makeClassNameMap () {
            Map map = new HashMap();
            int cLength = _children.length;
            for (int i = 0; i < cLength; i++) {
                File path = _children[i];
                if (path.isFile ()) {
                    String className = path.getName ();
                    if (className.endsWith (".class")) {
                        try {
                            ClassReader cr = new ClassReader(ClassPathManager.loadFile (path));
                            className = cr.getClassName().intern();
                            map.put (ClassName.makeClassName(className), className);
                        } catch (ArrayIndexOutOfBoundsException e) {
                            Usages.message (e + " " + path);
                        }
                    }
                }
            }
            return map;
        }
    }
    
}
