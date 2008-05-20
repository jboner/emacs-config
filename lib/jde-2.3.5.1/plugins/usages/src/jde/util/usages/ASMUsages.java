/*
 * $Id: ASMUsages.java,v 1.29 2006/04/07 22:47:38 surajacharya Exp $
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

import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import jde.util.Usages;
import jde.util.Usages.IUsages;
import jde.util.Usages.Location;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.Type;
import org.objectweb.asm.MethodVisitor;

public class ASMUsages  implements IUsages, Serializable {

    // Minimum time in milliseconds between calls to refresh().
    public static long TIME_BETWEEN_REFRESHES = 10000;
    
    public static interface Signature {

        public boolean matches (String methodName, Type retType, Type[] argTypes, boolean isField);

        public boolean isConstructor();
    }

    ClassPathManager classpath;
    String projectName;
    String projectClasspath;

    public ASMUsages (String projectName, String projectClasspath) {
        this.projectName = projectName.intern();
        this.projectClasspath = projectClasspath.intern();
        classpath = new ClassPathManager (this);
        classpath.buildCache();
        lastRefreshTime = System.currentTimeMillis();
    }


    private void findUsagesInClass (String cName, final ASMSignature m, final Matches matches) {
        final String className = cName.replace('.', '/').intern();
        Collection s = classpath.getDependencies (ClassName.makeClassName (className));
        if (s == null)
            s = new HashSet();

        for (Iterator i = s.iterator(); i.hasNext();) {
            final Object thisClass =  i.next();
            final ClassReader cr = classpath.getClassReader (thisClass);
            if (cr != null) {
                cr.accept(new NoOpClassVisitor() {
                        public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
//                             if (signature != null)
//                                 Usages.message (name + "." + desc + ":" + signature);
                            return new ASMCodeVisitor (access, cr.getClassName() /*thisClass*/, name, desc, exceptions, className, m, matches);
                        }
                    }, false);
            }

        }
    }

    // returns a List of Locations
    public  List findUsages (String className, Usages.Signature m, boolean strict) {
        className = getRealName (className);
        List locs = null;

        long start = System.currentTimeMillis();
        refresh();
        
        final ASMSignature asmM;
        className = className.intern();
        if (m instanceof Signature)
            asmM = (ASMSignature) m;
        else
            asmM = new ASMSignature (m);
        
        Set collect = new HashSet ();
        
        boolean isAbstract = false;
        ClassReader cr = classpath.getClassReader (className);

        if (cr != null && classpath.isStaticFinalVar (cr, asmM)) {
            Usages.string ("Cannot find usages for static finals");
            return null;
        }
        
        
        if (cr != null)
            isAbstract = ! classpath.implementsMethod (cr, asmM);
        
        if (strict || m.isConstructor())
            collect.add (className);
        else {
            classpath.getSupers (className, asmM, collect);
            
            if (isAbstract)
                classpath.getSubs (className, null, collect);
            else
                classpath.getSubs (className, asmM, collect);
        }

        System.out.println  ("(setq jde-usages-related-classes (quote ");
        Usages.print (System.out, collect);
        System.out.println  ("))");

        
        Matches matches = new Matches();
        for (Iterator i = collect.iterator();i.hasNext();){
            String relatedClass = (String) i.next();
            findUsagesInClass (relatedClass, asmM, matches);
        }
        
        locs = new LinkedList();
        for (Iterator i=matches.usages.keySet().iterator();i.hasNext();) {
            String clazz = (String) i.next();
            if (!(locs.size() == 0 &&  clazz.equals (className)))
                locs.add ('"' + clazz.replace ('/', '.') + '"');
            locs.addAll (matches.usages.getC (clazz));
        }
        long stop = System.currentTimeMillis();


        if (Usages.DEBUG) {
            Usages.message  ("\tfindUsages avg " + (stop-start));
        }

        return locs;
    }

    
    /**
     * Returns a tree of super-classes.
     *
     * @return a List of String or List objects
     */
    public  List findSupers (String className, boolean includeInterfaces) {
        refresh();
        className = getRealName (className);
        return classpath.getSupers (className, includeInterfaces);
    }
    
    public  List findSubs (String className, boolean getSubs, boolean getImpls) {
        refresh();
        className = getRealName (className);
        return (List) classpath.getSubs (className, getSubs, getImpls);
    }

    public  List findSubs (String className, Usages.Signature m) {
        final ASMSignature asmM;
        className = getRealName (className);
        if (m instanceof Signature)
            asmM = (ASMSignature) m;
        else
            asmM = new ASMSignature (m);
        refresh();
        return (List) classpath.getSubs (className, asmM);
    }

    public  void setProjectValues(String projectName, String projectClasspath) {
        this.projectClasspath = projectClasspath.intern();
        classpath.setProjectClasspath(projectClasspath);
    }

    public String getProjectClasspath () {
        return projectClasspath;
    }

    public Set getAllClasses () {
        // refresh(); Usages should always call refresh first to find out if a
        // call to getAllClasses is needed.
        return classpath.getAllClasses();
    }

    public ClassPathManager getClassPathManager () {
        return classpath;
    }

    public void shutdown() {
        classpath.writeToDiskCache();
    }

    public long lastRefreshTime = Long.MAX_VALUE;
    public void refresh () {
        long start = System.currentTimeMillis();

        // don't refresh if the last time we were called was not very long ago
        if ((lastRefreshTime <= start) && (start-lastRefreshTime <= TIME_BETWEEN_REFRESHES))
            return;
        
        classpath.refresh();
        long stop = System.currentTimeMillis();
        if (Usages.DEBUG && stop > start)
            Usages.message ("refresh : " + (stop -start) + " ms");
        lastRefreshTime = stop;
    }

    public List getQualifiedNames (String className) {
        refresh();
        return getClassPathManager().getQualifiedNames (className);
    }

    public String classExists (String className) {
        try {
            ClassLoader.getSystemClassLoader().loadClass (className);
            return className;
        }
        catch (ClassNotFoundException e) { }
        catch (NoClassDefFoundError e) { }
        
        String rName = getRealName (className);
        if (getClassPathManager().getClassLocation (rName) != null)
            return rName;
        else
            return null;
    }

    public String getClassWithSignatureDefinition (String className, Usages.Signature m) {
        final ASMSignature asmM;
        className = getRealName (className).intern();
        if (m instanceof Signature)
            asmM = (ASMSignature) m;
        else
            asmM = new ASMSignature (m);
        String defnClass = classpath.getClassWithSignatureDefinition (className, asmM);
        if (defnClass != null)
            defnClass = defnClass.replace ('/', '.');
        return defnClass;
    }
    

    /**
     * Replaces any of the '.' characters in className to '$' characters. This
     * is it converts inner class names like foo.bar.A.B to
     * foo.bar.A$B. Classes that are not inner classes are left unchanged.
     *
     * This is accomplished by searching in the classpaths first for className
     * and then replacing the last '.' in className by '$', searching for this
     * name in the classpaths and repeating till a match is found or until the
     * class name runs out of '.'s.
     *
     * @param className a <code>String</code> value
     * @return a <code>String</code> value
     */
    public String getRealName (String className) {
        String rClassName = className = className.replace ('.', '/').intern();
        while  (true) {
            if (classpath.classExists (rClassName)) {
                if (rClassName != className)
                    Usages.message ("Assuming " + className + " is really " + rClassName.replace ('/', '.'));
                return rClassName;
            }
            


            if (rClassName.lastIndexOf ('/') == -1)
                return className;
            
            // replace the last / with a $
            if (System.getProperty("java.version").startsWith ("1.3")) {
                int slash = rClassName.lastIndexOf ('/');
                if (slash > 1)
                    rClassName = (rClassName.substring (0,slash-1)  + "$" + rClassName.substring(slash+1)).intern();
            } else
                rClassName = rClassName.replaceFirst ("/([^/]*)$", "\\$$1").intern();

        }
    }


    public class Matches {
        ObjectToCollectionMap usages = new ObjectToCollectionMap();

        public void add (ASMCodeVisitor cv, int line, boolean write, boolean repeated) {
            Location loc = new Location (cv.getClassName(), cv.getMethodName(), cv.getSig(), line, write);
            if (repeated)
                count++;
            else
                count = 0;
            loc.occuranceCount = count;
            usages.addToCollection (cv.getMatchClass(), loc);
        }

        int count;

        public String toString () {
            return usages.toString();
        }

        public ObjectToCollectionMap getUsages () {
            return usages;
        }
    }
}
