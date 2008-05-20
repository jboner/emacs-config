/*
 * $Id: ClassPathManager.java,v 1.37 2006/05/05 22:17:01 surajacharya Exp $
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

import jde.util.Usages;
import org.objectweb.asm.*;

import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.io.Serializable;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

public  class ClassPathManager  implements Serializable {

    private List _paths;
    private Map _definitions;
    private boolean _cacheInvalid;
    private Map _classpathEntries;

    // The subclass relation for all classes in the classpath. The set of
    // subclasses for a class may have extra entries if a classpath entry has
    // been deleted, because we check if this relation in
    // SubClassVisitor.visitSubs. This allows us to do less when refreshing.
    private ObjectToCollectionMap _subs = new ObjectToCollectionMap(TreeSet.class);
    private ASMUsages usages;
    public static int POOL_SIZE = 1;
        
        
    ClassPathManager (ASMUsages usages) {
        this.usages = usages;
        _paths = new LinkedList();
        _definitions = new HashMap();
        _classpathEntries = new HashMap();
        setProjectClasspath (usages.getProjectClasspath());
        // markCacheInvalid(); setProjectClasspath marks the cache invalid
    }

    public void setProjectClasspath (String projectClassPath) {
        // Remove associations between the old classpath entries and this
        // manager. Only the top-level cpe's are touched because we assume the
        // inner ones share the classpath manager list with their parents
        for (Iterator i = _paths.iterator();i.hasNext();) {
            String path = ((File) i.next()).getPath().intern();
//            File path = (File) i.next();
            ClassPathEntry cpe = (ClassPathEntry) _classpathEntries.get (path);
            if (cpe != null)
                cpe.removeClassPathManager (this);
        }

        _paths.clear();
        _classpathEntries.clear();
        _definitions.clear();
        
        StringTokenizer st = new StringTokenizer(projectClassPath, java.io.File.pathSeparator);
        while (st.hasMoreTokens()) {
            File path = new File (st.nextToken());
            _paths.add (path);
        }
        setCacheInvalid(true);
        Usages.classListChanged (usages);
    }

    public boolean isCacheInvalid () {
        return _cacheInvalid;
    }
    
    public void setCacheInvalid(boolean status) {
        _cacheInvalid = status;
    }


    public Collection getDependencies (Object className) {
        HashSet deps = new HashSet();
        for (Iterator i = _classpathEntries.values().iterator();i.hasNext();) {
            ClassPathEntry cpe = (ClassPathEntry) i.next();
            cpe.getPossibleUsers (className, deps);
        }
        return deps;
    }


    public ClassReader getClassReader (String classname) {
        return getClassReader (ClassName.makeClassName (classname));
    }

    public ClassReader getClassReader (Object classname) {
        byte[] bytes = getBytes (classname);
        if (bytes == null) {
            // If this is a class in the jdk we try to get the .class file from
            // the system classpath. This allows you to omit the jdk jar from
            // jde-global-classpath
            String classnameS = (ClassName.getClassNameString (classname));
            if (classnameS != null && (classnameS.startsWith("java/") || classnameS.startsWith ("javax/"))) {
                InputStream in = ClassLoader.getSystemClassLoader().getResourceAsStream (classnameS + ".class");
                if (in != null) {
                    try {
                        return new ClassReader (in);
                    } catch (IOException e) {
                        Usages.message (e);
                    }
                }
            }
        } else {
            return new ClassReader (bytes);
        }
        if (Usages.DEBUG)
            Usages.message ("Can't find " + classname + " in " + _paths.toString().replace ('\\', '/'));
        else
            Usages.message ("Can't find " + classname + " in classpath.");
        return null;
    }
    


    public byte[] getBytes (Object classname) {
        if (isCacheInvalid()) {
            buildCache();
        }
        ClassPathEntry cpe = (ClassPathEntry) _definitions.get (classname);

        if (cpe == null) {
            return null;
        }

        return cpe.getBytes(classname);

    }

    /*
      Check if data for this classpath entry is cached before reading it.
    */
    public  void buildCache () {
        _definitions.clear();
        _classpathEntries.clear();
        _subs.clear();
        int order = 0;

//         final List<ClassPathEntry> cpes = new LinkedList<ClassPathEntry>();

        ThreadPoolManager pool = new ThreadPoolManager(POOL_SIZE);
        for (Iterator i = _paths.iterator();i.hasNext();) {
            final File file = (File) i.next();
            final int thisOrder = order;
            pool.submit (new Runnable () {
                           public void run () {
                               if (!Usages.DEBUG)
                                   Usages.message ("processing " + file.getPath().replace('\\','/') + "...");
                               long start = System.currentTimeMillis();
                               ClassPathEntry cpe = ClassPathEntry.get (file);
                               try {
                                   cpe.addClassPathManager (ClassPathManager.this, thisOrder);
                                   cpe.refresh();
                                   ClassPathManager.this.addEntryAndRebuildSubsAndDeps (cpe);
//                                    synchronized (cpes) {
//                                        cpes.add (cpe);
//                                    }
                               } catch (IOException e) {
                                   Usages.message (e);
                               }
                               if (Usages.DEBUG)
                                   Usages.message ( file.getPath().replace('\\','/') + " = " + (System.currentTimeMillis() - start)  + " ms");
                           }
                       });
            order++;
        }

       
        pool.waitTillAllDone ();

//         for (Iterator i = cpes.iterator(); i.hasNext() ;)
//             addEntryAndRebuildSubsAndDeps ((ClassPathEntry) i.next());
        
        setCacheInvalid (false);
    }

    public List getQualifiedNames (String className) {
        ArrayList l = MyClassLoader.getQualifiedNames (className);
        className = "/" + className;
        for (Iterator i = _definitions.keySet ().iterator ();i.hasNext ();) {
            String clazz = (String)i.next();
            if (clazz.endsWith (className))
                l.add (clazz.replace('/','.'));
        }
        return l;
    }
    

    public static class MyClassLoader extends ClassLoader {
        static MyClassLoader singleton;
        public static ArrayList getQualifiedNames (String className) {
            synchronized (MyClassLoader.class) {
                if (singleton == null)
                    singleton = new MyClassLoader();
            }
            ClassLoader sys = ClassLoader.getSystemClassLoader();
            Package[] packages = singleton.getPackages();
            ArrayList l = new ArrayList ();
            for (int i=0; i < packages.length;i++) {
                String name = packages[i].getName() + "." + className;
                try {
                    sys.loadClass (name);
                    l.add (name);
                } catch (ClassNotFoundException e) {} catch (NoClassDefFoundError e) {}
            }
            return l;   
        }
    }
    
    
    public void rebuildSubsAndDeps (ClassPathEntry cpe) {
        synchronized (_definitions) {
            for (Iterator j = cpe._classes.iterator();j.hasNext();) {
                setClassLocation (j.next(), cpe);
            }
        }

        synchronized (_subs) {
            for (Iterator j = cpe._subs.entrySet().iterator();j.hasNext();) {
                Map.Entry e = (Map.Entry) j.next();
                _subs.addAllToCollection (e.getKey(), (Collection)e.getValue());
            }
        }
        
        for (Iterator i = cpe.getChildren().iterator();i.hasNext();) {
            rebuildSubsAndDeps ((ClassPathEntry) i.next());
        }
    }

    public void writeToDiskCache () {
        for (Iterator i = _paths.iterator();i.hasNext();) {
            File file = (File) i.next();
            ClassPathEntry cpe = (ClassPathEntry) _classpathEntries.get (file.getPath().intern());
            if (cpe != null) {
                ClassPathEntry.cacheCPE (cpe);
            }
        }
    }
    

    public static void print (byte[] b) {
        System.out.print ("b = " );
        for (int i = 0; i < 100 && i < b.length ; i++)
            System.out.print (b[i] + " " );
        System.out.println  ();
    }

    private void addToList (List l1, List l2) {
        switch (l2.size()){
        case 0 : break;
        case 1: l1.add (l2.get(0));break;
        default : l1.add (l2);
        }
    }
    
    /**
     * Looks at the byte-code to find the superclass and implemented interfaces
     * for <code>className</code>.
     * Accumulates in <code>collect</code> the ancestors of class
     * <code>className</code> for which method (or field) <code>m</code> is
     * defined. Ie for an object o of a class in <code>collect</code> the
     * method-call/field-access </code>o.m</code> must be legal.
     *
     * @param className a <code>String</code> value
     * @param collect a <code>Set</code> value
     */
    public boolean getSupers (String className, final ASMUsages.Signature m, Set collect) {

        if (className == null)
            return false;
        className = className.intern();

        final boolean [] matchFound = { false };
        Collection supersSet = new LinkedList();
        ClassReader cr = getClassReader (className);
        
        if (cr == null)
            return false;

        String superClass = cr.getSuperClassName();
        if (superClass != null) {
            supersSet.add (superClass.intern());
        }
        String [] ints = cr.getImplementedInterfaces();
        for (int i =0; i < ints.length; i++) {
            supersSet.add (ints[i].intern());
        }
        if (m==null) {
            for (Iterator i =supersSet.iterator(); i.hasNext();)
                getSupers ((String)i.next(), null, collect);
            collect.add (className);
            return true;
        } else {
            cr.accept (new NoOpClassVisitor () {
                    public FieldVisitor visitField(int n, String name, String desc, String sig, Object object) {
                        if (m.matches (name, Type.getReturnType(desc), new Type[0], true))
                            matchFound[0] = true;
                        return null;
                    }
                
                    public MethodVisitor visitMethod(int n, String name, String desc, String sig, String[] stringArray) {
                        if (m.matches (name, Type.getReturnType(desc), Type.getArgumentTypes(desc), false))
                            matchFound[0] = true;
                        return null;
                    }
                }, true);
        
            for (Iterator i =supersSet.iterator(); i.hasNext();)
                if (getSupers ((String)i.next(), m, collect))
                    matchFound[0] = true;

            if (matchFound[0])
                collect.add (className);

            return matchFound[0];
        }
    }

    private interface SubClassVisitor {
        public boolean visit (String clazz, String subclazz, ClassReader cr);
        public void preVisit (String className, int size);
        public void postVisit (String className, int size);
    }

    private void visitSubs (String className, SubClassVisitor v)  {
        
        className = className.intern();
        Object classNameS = ClassName.makeClassName (className);
        Collection subsSet = (Collection) _subs.get (classNameS);
        if (subsSet == null) {
            v.preVisit (className, 0);
            v.postVisit (className, 0);
            return;
        }
        v.preVisit (className, subsSet.size());
        for (Iterator i = subsSet.iterator(); i.hasNext();) {
            Object sub = i.next();
 
            ClassReader cr = getClassReader(sub);

            if (cr == null || !isSubclass(className, cr)) {
                // the subclass relation is no longer true
                i.remove();
                continue;
            }

            if (v.visit (className, cr.getClassName(), cr)) {
                visitSubs (cr.getClassName(), v);
            }

        }
        v.postVisit (className, subsSet.size());
    }

    private interface SubClassVisitor2 {
        public boolean enter (String clazz, ClassReader cr, int size);
        public void leave (String clazz, ClassReader cr, int size);

    }

    public static class  SubClassNoOpVisitor implements SubClassVisitor2 {
        public boolean enter (String clazz, ClassReader cr, int size) {return true;}
        public void leave (String clazz, ClassReader cr, int size) {}
    }
    
    private void visitSubs (String className, SubClassVisitor2 v)  {
        className = className.intern();
        visitSubs (className, v, getClassReader(className));
    }
    
    private void visitSubs (String className, SubClassVisitor2 v, ClassReader cr)  {
        if (cr == null)
            return;

        Object classNameS = ClassName.makeClassName (className);
        Collection subsSet = (Collection) _subs.get (classNameS);
        int size = subsSet != null ? subsSet.size() : 0;
        boolean visitChildren = v.enter (className, cr, size);
        
        if (visitChildren && subsSet != null) {
            for (Iterator i = subsSet.iterator(); i.hasNext();) {
                Object subS =  i.next();
 
                ClassReader cr2 = getClassReader(subS);
                
                if (cr2 == null || !isSubclass(className, cr2)) {
                    // the subclass relation is no longer true
                    i.remove();
                    continue;
                }

                visitSubs (cr2.getClassName(), v, cr2);
                                
            }
        }

        if (visitChildren)
            v.leave (className, cr, size);
        
    }
    
    /**
     *   Returns true if the class <code>cr</code> overrides method <code>m</code>
     *
     * @param cr a <code>ClassReader</code> value
     * @param m an <code>ASMSignature.Signature</code> value
     * @return a <code>boolean</code> value
     */
    boolean implementsMethod (ClassReader cr, final ASMUsages.Signature m) {
        final boolean [] matchFound = { false };
        cr.accept (new NoOpClassVisitor () {
                public FieldVisitor visitField(int n, String name, String desc, String sig, Object object) {
                    if (m.matches (name, Type.getReturnType(desc), new Type[0], true))
                        matchFound[0] = true;
                    return null;
                }

                public MethodVisitor visitMethod(int n, String name, String desc, String sig, String[] stringArray) {
                    // don't include abstract methods        // method sig must match also
                    if ((n & Opcodes.ACC_ABSTRACT) == 0 && m.matches (name, Type.getReturnType(desc), Type.getArgumentTypes(desc), false))
                        matchFound[0] = true;
                    return null;
                }
            }, true);
        return matchFound[0];   
    }

    /**
     *   Returns true if <code>m</code> is a static final variable in class <code>cr</code>
     *
     * @param cr a <code>ClassReader</code> value
     * @param m an <code>ASMSignature.Signature</code> value
     * @return a <code>boolean</code> value
     */
    boolean isStaticFinalVar (ClassReader cr, final ASMUsages.Signature m) {
        // If there are multiple members that match m, should return true only
        // if all of them are static finals.

        // true if one of the matching members is a static final
        final boolean [] atLeastOneStaticFinal = { false };

        // false if one of the mathing members is not a static final
        final boolean [] allMatchesStaticAndFinal = { true };

        // We need both these booleans because allMatchesStaticAndFinal is true
        // when there are no matches for m in cr
        
        cr.accept (new NoOpClassVisitor () {
                public FieldVisitor visitField(int n, String name, String desc, String sig, Object object) {
                    if (m.matches (name, Type.getReturnType(desc), new Type[0], true) )
                        if ((n & Opcodes.ACC_STATIC) != 0 && (n & Opcodes.ACC_FINAL) != 0)
                            atLeastOneStaticFinal[0] = true;
                        else
                            allMatchesStaticAndFinal[0] = false;
                    return null;
                }
            }, true);
        return  atLeastOneStaticFinal[0] && allMatchesStaticAndFinal[0];
    }
    

    /**
     * Returns a tree of super-classes.
     *
     * @return a List of String or List objects
     */
    public List getSupers (String className, boolean includeInterfaces) {
        List returnList = new LinkedList();

        if (className == null)
            return returnList;
        className = className.intern();

        ClassReader cr = getClassReader (className);
        
        if (cr == null)
            return returnList;

        returnList.add (getClassObject (className.replace('/', '.'), cr));

        String superClass = cr.getSuperClassName();
        if (superClass != null && !superClass.equals ("java/lang/Object")) {
            addToList (returnList, getSupers (superClass, includeInterfaces));
        }
        if (includeInterfaces){
            String [] ints = cr.getImplementedInterfaces();
            for (int i =0; i < ints.length; i++) {
                addToList (returnList, getSupers (ints[i], includeInterfaces));;
            }
        }
        
        return returnList;
    }


    public String getClassWithSignatureDefinition (String className, ASMUsages.Signature m) {
        if (className == null) {
            return null;
        }
        
        String clazz;

        // check for definition in superclasses
        ClassReader cr = getClassReader (className.intern());
        if (cr == null)
            return null;
        
        String superClass = className;
        ClassReader superCr = cr;
        while (superCr != null){
            if (implementsMethod(superCr, m)) {
                return superClass;
            }
            if (superClass.equals ("java/lang/Object")) {
                break;
            }            
            superClass = superCr.getSuperClassName();
            superCr = getClassReader (superClass);
        }

        // check interfaces
        
        String ints[] = cr.getImplementedInterfaces();
        for (int i = 0;i < ints.length;i++) {
            clazz = getClassWithSignatureDefinition (ints[i].intern(), m);
            if (clazz != null)
                return clazz;
        }

        // check outer classes (ie className might be an inner class)
        int index  = className.lastIndexOf ('$');
        while (index != -1) {
            clazz = getClassWithSignatureDefinition (className.substring (0, index), m);
            if (clazz != null)
                return clazz;
            index  = className.lastIndexOf ('$', index-1);
        }

        return null;
    }


    /**
     * Find subclasses that do not implement the method we are looking
     * for. Returns a flat collection.
     *
     * @param className a <code>String</code> value
     * @param m a <code>ASMSignature</code> value
     * @param collect a <code>Set</code> value
     */
    void  getSubs (final String className, final ASMUsages.Signature m, final Collection collect) {
        collect.remove(className.intern());
        visitSubs (className, new SubClassNoOpVisitor () {
                public boolean enter (String clazz, ClassReader cr, int size) {
                    // the first time we are called clazz is the same as
                    // className, so return true to force looking at its
                    // subclasses
                    if (className == clazz) 
                        return true;
                    if (collect.contains (clazz)) 
                        return false; // seen this class before
                    if (m == null || !implementsMethod (cr, m)) {
                        collect.add (clazz);
                        return true;
                    } else {
                        return false;
                    }
                }
            });
        collect.add(className.intern());
    }


    /**
     * Returns a structure which represents the class and interface hierarchy
     * tree rooted at <code>className</code>. If you define a child entity of
     * <code>className</code> as being either a class or interface which can be
     * a) a subclass of <code>className</code> or
     * b) a subinterface of <code>className</code> or
     * c) a class implementing <code>className</code>, 
     * then the return value of this function when printed should specify a list
     * of all the names of children of <code>className</code>.  If any child has
     * such children itself then it is represented as a list whose first element
     * is the name of the class and the the tail is the list of its children as
     * returned by getSubs.
     * 
     * @param className a <code>String</code> value
     * @param getSubs return subclasses of this class <code>className</code>,
     * if it is an interface return all subinterfaces.
     * @param getImpls return all classes implementing the interface <code>className</code>
     */
    public Object getSubs (String className, final boolean getSubs, final boolean getImpls) {
        final LinkedList stack = new LinkedList(); stack.add (new LinkedList());
        
        visitSubs (className, new SubClassVisitor2 () {

                public boolean enter (String clazz, ClassReader cr, int size) {
                    
                    // this is a sublcass, subinterface or a implementing class
                    // check if getSubs or getImpls let us add this
                    // class/interface
                    boolean isSub = cr.getSuperClassName().equals (clazz);
                    boolean match = (isSub && getSubs || !isSub && getImpls);

                    if (match) {
                        Object o = getClassObject (clazz, cr);

                        if (size == 0) {
                            ((LinkedList) stack.getLast()).add (o);
                            // no children to visit
                            return false;
                        }
                        
                        List newL = new LinkedList();
                        newL.add (o);
                        stack.add (newL);
                    }
                    
                    return match;
                }
                
                public void leave (String clazz, ClassReader cr, int size) {
                    LinkedList newL = (LinkedList) stack.removeLast();
                    if (newL.size() > 1) 
                        ((LinkedList) stack.getLast()).add (newL);
                    else
                        ((LinkedList) stack.getLast()).add (newL.getLast());
                }
                
            });
        
        
        LinkedList l =  (LinkedList) stack.getLast();
        if (l.size() == 0) // input class does not exist
            return null;

        if (! (l.getLast() instanceof List))
            return null;
        else 
            return l.getLast();
    }

    /**
     * Returns a representation of class <code>cr</code> suitable for printing.
     * If cr is an interface, this method returns a Usages.Interface object, if
     * it is a abstract class this method returns a Usages.AbstractClass object
     * otherwise it returns a string.
     *
     * @param clazz a <code>String</code> value
     * @param cr a <code>ClassReader</code> value
     * @return an <code>Object</code> value
     */
    private Object getClassObject (String clazz, ClassReader cr) {
        Object o = clazz;
        if (cr.isInterface())
            o = new Usages.Interface (clazz);
        else if (cr.isAbstract ())
            o = new Usages.AbstractClass (clazz);
        return o;
    }


    /**
     * Returns all subclasses of className which override method m. If
     * className is a interface this menthod returns all implementing
     * classes which define or overide m.
     *
     * @param className a <code>String</code> value
     * @param m an <code>ASMUsages.Signature</code> value
     * @return an <code>Object</code> value
     */
    public Object getSubs (String className, final ASMUsages.Signature m) {
        final LinkedList stack = new LinkedList();
        List l1 = new LinkedList();
        List l2 = new LinkedList();
        l2.add (className);
        l1.add (l2);
        stack.add (new LinkedList());
        stack.add (l1);
        visitSubs (className, new SubClassVisitor () {
                public void preVisit (String clazz, int size) {
                }
                public boolean visit (String clazz, String subclazz, ClassReader cr) {
                    stack.add (new LinkedList());
                    Object o = getClassObject (subclazz, cr);

                    if (implementsMethod (cr, m)) {
                        List innerL = new LinkedList();
                        innerL.add (o);
                        o = innerL;
                    } 
                    ((LinkedList) stack.getLast()).add (o);
                    return true;
                }
                public void postVisit (String clazz, int size) {
                        LinkedList l = (LinkedList) stack.removeLast();
                        if (l.size() == 1) {
                            if (! (l.getLast() instanceof LinkedList)) {
                            } else {
                                ((LinkedList) stack.getLast()).add(l.getLast());
                            }   
                        } else {
                            ((LinkedList) stack.getLast()).add(l);
                        }
                }
                    
            });
        LinkedList l = (LinkedList) ((LinkedList) (LinkedList) stack.getLast()).getLast();
        if (l.size() == 1)
            return null;
        else
            return l;
            
    }
    
    

    /**
     * Checks if the class represented by <code>ClassReader</code> @param is a
     * subclass or subinterface of class @param className.
     *
     * @return true if cr is a subclass of className.
     */
    public boolean isSubclass(String className, ClassReader cr) {
        boolean isSubclass = false;
        String superClass = cr.getSuperClassName();
        if (superClass.equals (className))
            isSubclass = true;
        if (!isSubclass) {
            String [] ints = cr.getImplementedInterfaces();
            for (int in =0; in < ints.length; in++) {
                superClass = ints[in].intern();
                if (superClass.equals (className))
                    isSubclass = true;
            }
        }
        return isSubclass;
    }

    // shared byte buffer, all calls to loadFile use this buffer
    // private static byte[] b = null;
    
    public static byte[] loadFile(File argFile) {
        if (argFile.exists()) {
            try {
                return read (new BufferedInputStream (new FileInputStream(argFile)), (int)argFile.length());
            } catch (FileNotFoundException e) {
                Usages.message (e);
            } catch (IOException e) {
                Usages.message (e);
            } 
        }
        return null;
    }

    public static byte[] loadFile (ZipFile argFile, String argClassName) {
        //zip and jar files seems to always be separated by a '/'
        argClassName = argClassName.replace(File.separatorChar, '/');
        ZipEntry ze = argFile.getEntry(argClassName);
        return loadFile (argFile, ze);
    }


    public static byte[] loadFile(ZipFile argFile, ZipEntry ze) {
        try {
            if (ze != null) {
                return read (argFile.getInputStream(ze), (int) ze.getSize());
            }
        } catch (IOException e) {
            Usages.message (e);
        }
        return null;
    }


    private static byte[] read (InputStream is, int size) throws IOException {
        int len = 0;
        byte [] b = new byte[size];
        try {
            while (true) {
                int n = is.read(b, len, size - len);
                if (n == -1 || n == 0) {
                    if (len < size) {
                        Usages.message  (is + ":read = " + len + " expected = " + size);
                    }
                    break;
                } else
                    len += n;
            }
        } finally {
            try {
                is.close();
            } catch (IOException e) {Usages.message (e);} // ignore
        }        
        return b;
    }

    
    public void addEntryAndRebuildSubsAndDeps (ClassPathEntry cpe) {
            String  path = cpe.getStringPath();
            synchronized (_classpathEntries) {
                _classpathEntries.put (path, cpe);
            };
            rebuildSubsAndDeps (cpe);
   }

    public ClassPathEntry getEntry (String path) {
            return (ClassPathEntry) _classpathEntries.get (path);
    }

    public void setClassLocation (Object className, ClassPathEntry cpe) {
        synchronized (_definitions) {
            ClassPathEntry cpe2 = (ClassPathEntry)_definitions.get (className);
            if (cpe2 == null) {
                // new class added
                Usages.classAdded (usages, ClassName.getClassNameString (className));
            }
            if (cpe2 == null || cpe2.getOrder (this) > cpe.getOrder (this))
                _definitions.put (className, cpe);
        }
    }

    public ClassPathEntry getClassLocation (String className) {
        return (ClassPathEntry)_definitions.get (className);
    }

    public boolean classExists (String className) {
        if (getClassLocation (className) != null)
            return true;
        if (ClassLoader.getSystemResource (className) != null)
            return true;
        return false;
    }

    public static boolean USE_POOL = true;
    
    public void refresh () {
        if (isCacheInvalid()) {
            buildCache();
        } else {

            ThreadPoolManager pool = new ThreadPoolManager(POOL_SIZE);
        
            for (final Iterator i = new LinkedList (_classpathEntries.values()).iterator();i.hasNext();) {
                final ClassPathEntry  cpe = (ClassPathEntry) i.next();
                if (USE_POOL) {
                    pool.submit (new Runnable () {
                            public void run () {
                                try {
                                    cpe.refresh();
                                } catch (IOException e) {
                                    Usages.message (e);
                                }
                            }
                        });
                } else {
                    try {
                        ((ClassPathEntry) i.next()).refresh();
                    } catch (IOException e) {
                        Usages.message (e);
                    }
                }
                
            }

            if (USE_POOL)
                pool.waitTillAllDone();

                
        }        
                
    }

    public Collection getClasses (ClassPathEntry cpe) {
        LinkedList classes = new LinkedList();
        for (Iterator i = _definitions.entrySet().iterator(); i.hasNext();) {
            Map.Entry e = (Map.Entry) i.next();
            String className = (String) e.getKey();
            ClassPathEntry cpe2 = (ClassPathEntry) e.getValue();
            if (cpe == cpe2)
                classes.add (className);
        }
        return classes;
    }

    public Set getAllClasses () {
        return _definitions.keySet();
    }

    public void findNewLocations (Set classes, int oldOrder) {
        for (Iterator i = classes.iterator(); i.hasNext();) {
            String className = (String) i.next();
            ClassPathEntry cpe = (ClassPathEntry)_definitions.get (className);
            if (cpe != null && cpe.getOrder (this) < oldOrder)
                i.remove ();
        }

        List sortedCPEs = new LinkedList (_classpathEntries.values());
        Collections.sort(sortedCPEs, new Comparator () {
                public int compare (Object o1, Object o2) {
                    return ((ClassPathEntry) o1).getOrder (ClassPathManager.this) - ((ClassPathEntry) o2).getOrder (ClassPathManager.this) ;
                }
            });


        for (Iterator i = sortedCPEs.iterator();i.hasNext() && classes.size() > 0;) {
            ClassPathEntry cpe = (ClassPathEntry) i.next();
            if (cpe.getOrder (this) <= oldOrder)
                continue;

            for (Iterator j = classes.iterator(); j.hasNext();) {
                String className = (String) j.next();
                if (cpe._classes.contains (className)) {
                    j.remove ();
                    if (Usages.DEBUG) {
                        if (!System.getProperty("java.version").startsWith ("1.3"))
                            Usages.message (className + " removed from " + _definitions.get (className)  + ", found alternate definition in " + cpe._path + ".");
                        else
                            Usages.message (className.replaceAll("[/$]", ".") + " removed from " + _definitions.get (className)  + ", found alternate definition in " + cpe._path + ".");
                    }
                    _definitions.put (className, cpe);
                }
            }
        }


        for (Iterator i = classes.iterator(); i.hasNext();) {
            String className = (String) i.next();
            _definitions.remove (className);
            // class removed
            Usages.classRemoved (usages, className);
//           if (Usages.DEBUG)
//                 Usages.message ("Class " + className + " removed from the classpath");
        }
    }
    
}
