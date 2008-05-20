 /*
 * $Id: Usages.java,v 1.40 2006/05/05 20:53:42 surajacharya Exp $
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
package jde.util;

import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jde.util.usages.ASMSignature;
import org.objectweb.asm.Type;


/**
 * The jde-usages interface to emacs (via the beanshell). The only reason this
 * class is in the jde.util package is that it needs to get the class path for
 * the current project from ProjectClasses which is a package protected class. 
 *
 * @version 1.0
 */
public class Usages {
    
    public static boolean DEBUG = false;
    public static int MIN_TIME = 10;

    public static Map _usages = new HashMap();
    public static Map _allClassesListNeedsRefreshing = new HashMap();

    static {
        Runtime.getRuntime().addShutdownHook(new Thread() {
                public void run () {
                    for (Iterator i = _usages.values().iterator();i.hasNext();) {
                        ((IUsages) i.next()).shutdown();
                    }
                }
            });
    }

    /**
     * The interface to be provided by any subsystem than can provide usage
     * info. The intention of this was to insulate jde.util.Usages and hence the
     * elisp code from the choice of the bytecode library. Currently the only
     * implementation is jde.util.usages.ASMUsages which uses the ASM bytecode
     * library.
     */
    public interface IUsages {

        public  List findUsages (String className, Signature m, boolean strict);

        public  void setProjectValues(String projectName, String projectclassPath);

        public String getProjectClasspath ();

        public List findSubs (String className, boolean getSubs, boolean getImpls);

        public  List findSubs (String className, Signature m);

        public  List findSupers (String className, boolean includeInterfaces);

        public List getQualifiedNames (String className);

        public String classExists (String className);

        public String getClassWithSignatureDefinition (String className, Signature m);

        public Set getAllClasses();

        public void refresh ();
        
        public void shutdown();
        
    }


    
    /**
     * A method signature filter. <code>matches</code> returns true is the
     * method signature described in its paramaters is a method this search is
     * interested in.
     *
     */
    public static interface Signature {

        public boolean matches (String methodName, String retType, String[] argTypes, boolean isField);

        public boolean isConstructor();

    }


    public static class Location {

        public Location (String className, String methodName, String signature, int lineNumber, boolean write) {
            this.lineNumber = lineNumber;
            this.className = className.replace('/', '.');
            this.methodName = methodName;
            this.signature = signature;
            this.write = write;
        }

        public String toString() {
            StringBuffer sb = new StringBuffer();

            sb.append ("(\"").append (className).append ("\" \"").append (methodName).append ("\" \"");
            sb.append (getTypeString (Type.getReturnType (signature))).append ("\" ");

            Type [] args = Type.getArgumentTypes(signature);
            if (args.length == 0)
                sb.append ("nil ");
            else {
                sb.append ("(");
                for (int i = 0; i < args.length;i++)
                    sb.append ("\"").append (getTypeString (args[i])).append ("\"");
                sb.append (") ");
            }

            sb.append (lineNumber);

            if (write)
                sb.append (" t ");
            else
                sb.append (" nil ");

            sb.append (occuranceCount);
            
            sb.append (" )");
            return sb.toString();
        }

        public int lineNumber;
        public int occuranceCount;
        public String className;
        public String methodName;
        public String signature;
        public boolean write;

    }


    public static void findUsages (String className, final Signature m, boolean strict, PrintStream out) {
        long start = System.currentTimeMillis();

        IUsages u  = getCurrentIUsages();

        List results = u.findUsages (className, m, strict);
        long stop = System.currentTimeMillis();

        if (results != null) {
            if (DEBUG && stop > start + MIN_TIME) {
                message ("total " + (stop - start));
            }
            
            out.println  ("(quote (");
            for (Iterator i = results.iterator();i.hasNext();)
                out.println  (i.next());
            out.println  ("))");
        }
        
    }

    public static void findSubs (String className, final Signature m, PrintStream out) {
        long start = System.currentTimeMillis();

        IUsages u = getCurrentIUsages();

        List results = u.findSubs (className, m);
        long stop = System.currentTimeMillis();

        if (DEBUG && stop > start + MIN_TIME) {
            message ("total " + (stop - start));
        }
        
        out.println  ("(quote ");
        print (out, results);
        out.println  (")");

    }

    public static void findSubs (String className, boolean getSubs, boolean getImpls, PrintStream out) {
        long start = System.currentTimeMillis();

        IUsages u  = getCurrentIUsages();

        List results = u.findSubs (className, getSubs, getImpls);

        long stop = System.currentTimeMillis();

        if (DEBUG) {
            message ("total " + (stop - start));
        }

        out.println  ("(quote ");
        print (out, results);
        out.println  (")");

    }

    public static void findSupers (String className, PrintStream out) {
        long start = System.currentTimeMillis();

        IUsages u  = getCurrentIUsages();

        List results = u.findSupers (className, true);

        long stop = System.currentTimeMillis();

        if (DEBUG) {
            message ("total " + (stop - start));
        }

        out.println  ("(quote ");
        print (out, results);
        out.println  (")");
    }

    public static void getAllClasses (PrintStream out) {
        long start = System.currentTimeMillis();

        IUsages u  = getCurrentIUsages();
        
        u.refresh();

        if (out == System.out || out == null) {
            // just return whether a refresh of the class list is required
            Object refresh = _allClassesListNeedsRefreshing.get (u);
            if (refresh == Boolean.FALSE) {
                System.out.println ("nil");
            } else {
                System.out.println ("t");
            }
        } else {
            Set results = u.getAllClasses ();
            _allClassesListNeedsRefreshing.put (u, Boolean.FALSE);
            long stop = System.currentTimeMillis();

            start = stop;
            out.println  ("(");
            for (Iterator i = results.iterator();i.hasNext();) {
                String  clazz = (String) i.next();
                clazz = clazz.replace('/', '.');
                // find the class name, could use a regexp here but that wouldn't
                // work with earlier jdks
                int index =  clazz.lastIndexOf('.');
                int index2 = clazz.lastIndexOf('$');
                String packageName = null;
                if (index2 > index) {
                    index = index2;
                    packageName = clazz.substring (0, index+1);
                } else if (index != -1) {
                    packageName = clazz.substring (0, index);
                }
                String uqName = clazz.substring (index+1);

                // skip over anonymous inner classes
                if (!System.getProperty("java.version").startsWith ("1.3"))
                    if (uqName.matches ("^[0-9]+$"))
                        continue;
            
                if (packageName != null)
                    out.print ("\"" + uqName + " (in " + packageName + ")\"");
                else
                    out.print ("\"" + clazz + "\"");

            };
            out.println  (")");
            stop = System.currentTimeMillis();
            if (DEBUG)
                message ("getAllClasses : " + (stop - start) + " ms");
        }
    }

    public static void print (PrintStream out, Object o) {
        out.println (o.toString());
    }

    public static void print (PrintStream out, String str) {
        out.println ("\"" + str + "\"");
    }

    public static void print (PrintStream out, Collection list) {
        out.println  ("(");
        if (list != null)
            for (Iterator i = list.iterator();i.hasNext();) {
                Object o = i.next();
                if (o instanceof Collection)
                    print (out, ((Collection) o));
                else if (o instanceof String)
                    print (out, ((String) o));
                else
                    print (out, o);
            }

        out.println  (")");
    }


    public static class Class {
        String className;
        public Class (String className) {
            this.className = className;
        }

        public String toString() {
            return ("\"" + className + "\"");
        }
    }
    
    public static class AbstractClass extends Class {
        public AbstractClass (String classname) {super (classname);}
        public String toString() {
            return "[" + super.toString() + " abstract]";
        }
    }
    
    public static class Interface extends Class {
        public Interface (String classname) {super (classname);}
        public String toString() {
            return "[" + super.toString() + " interface]";
        }
    }
    

    public static void findUsages (String className, final String methodName, final int numParams, boolean isField, boolean strict, PrintStream out) {
        Signature m = new ASMSignature (methodName, numParams, isField);
        findUsages (className, m, strict, out);
    }

    public static void findUsages (String className, final String methodName, String [] uArgs, boolean isField, boolean strict, PrintStream out) {
        findUsages (className, new ASMSignature (methodName, uArgs, isField), strict, out);
    }

    public static void findUsages (String className, boolean strict, PrintStream out) {
        findUsages (className, ASMSignature.MATCH_ALL_MEMBERS, strict, out);
    }
    
    public static void findSubs (String className, final String methodName, final int numParams, PrintStream out) {
        Signature m = new ASMSignature (methodName, numParams, false);
        findSubs (className, m, out);
    }

    public static void findSubs (String className, final String methodName, String [] uArgs, PrintStream out) {
        findSubs (className, new ASMSignature (methodName, uArgs, false), out);
    }

    public static void getQualifiedNames (String className) {
        StringBuffer result = null;

        IUsages u = getCurrentIUsages();
        result = new StringBuffer(JdeUtilities.START_PAREN);
        result.append(JdeUtilities.LIST);
        
        for (Iterator i = u.getQualifiedNames (className).iterator();
             i.hasNext();) {
            result.append(JdeUtilities.SPACE);
            result.append(JdeUtilities.DOUBLE_QUOTE);
            result.append(i.next().toString());
            result.append(JdeUtilities.DOUBLE_QUOTE);
        }
        result.append(JdeUtilities.END_PAREN);
        System.out.println(result.toString());
        System.out.flush();

    }

    public static void classExists (String className) {
        long start = System.currentTimeMillis();
        String exists = getCurrentIUsages().classExists (className);
        long stop = System.currentTimeMillis();

        if (DEBUG && stop > start + MIN_TIME)
            message ("classExists " +  " : " + className + " : " + (stop - start) + " ms");

        if (exists != null)
            string (exists);
        else
            System.out.println ( JdeUtilities.NIL );
        return;
    }

    
    public static void getClassWithSignatureDefinition (String className, Signature m, PrintStream out) {
        IUsages u = getCurrentIUsages();
        String result = u.getClassWithSignatureDefinition(className, m);
        if (result != null)
            print (out, result);
        else
            out.println (JdeUtilities.NIL);
    }
    
    public static void getClassWithSignatureDefinition (String className, final String methodName, final int numParams, boolean isField, PrintStream out) {
        Signature m = new ASMSignature (methodName, numParams, isField);
        getClassWithSignatureDefinition (className, m, out);
    }

    public static void getClassWithSignatureDefinition (String className, final String methodName, String [] uArgs, boolean isField, PrintStream out) {
        getClassWithSignatureDefinition (className, new ASMSignature (methodName, uArgs, isField), out);
    }


    public  static IUsages setProjectValues (String projectName, String projectClasspath) {
        projectName = projectName.intern();
        IUsages u = (IUsages) _usages.get (projectName);

        if (u == null) {
            u = new jde.util.usages.ASMUsages (projectName, projectClasspath); // use factory ?
            _usages.put (projectName, u);
        } else if (!u.getProjectClasspath().equals (projectClasspath))
            u.setProjectValues (projectName, projectClasspath);
        return u;
    }

    public static void classAdded (IUsages u, String className){
        classListChanged (u);
    }
    
    public static void classRemoved (IUsages u, String className){
        classListChanged (u);
    }

    public static void classListChanged (IUsages u) {
        _allClassesListNeedsRefreshing.put (u, Boolean.TRUE);
    }
    
    
    public static String getTypeString (Type t) {
        if (t.getSort() == Type.ARRAY)
            return getTypeString (t.getElementType()) + "[]";
        else if (t==Type.VOID_TYPE)
            return "<void>";
        else if (t==Type.BOOLEAN_TYPE)
            return "boolean";
        else if (t==Type.CHAR_TYPE)
            return "char";
        else if (t==Type.BYTE_TYPE)
            return "byte";
        else if (t==Type.SHORT_TYPE)
            return "short";
        else if (t==Type.INT_TYPE)
            return "int";
        else if (t==Type.FLOAT_TYPE)
            return "float";
        else if (t==Type.LONG_TYPE)
            return "long";
        else if (t==Type.DOUBLE_TYPE)
            return "double";
        else
            return t.getClassName();
    }

    public static void getClassInfo(String className, int level) {
        jde.util.usages.Completion.getClassInfo (className, level);
    }
    
    public static IUsages getCurrentIUsages () {
        long start = System.currentTimeMillis();
        String projectName = JdeUtilities.getCurrentProjectName().intern();
        String projectClasspath = JdeUtilities.getCurrentProjectClass().getClassPath().intern();

        IUsages u = setProjectValues (projectName, projectClasspath);
        long stop = System.currentTimeMillis();

        if (DEBUG && stop > start + MIN_TIME)
            message ("\tsetup " + (stop - start));

        return u;        
    }

    public static void message (Throwable e) {
        StringWriter s = new StringWriter();
        e.printStackTrace(new PrintWriter (s));
        message (s.toString());
    }

    public static void message (String msg) {
        System.out.println  ("(message \"" + msg.replace('\\','/') + "\")");
    }

    public static void string (String msg) {
        System.out.println ("(quote \"" + msg.replace('\\','/') + "\")");
    }

    public static void main (String [] args) {
        Usages.DEBUG=true;
        jde.util.usages.ClassPathManager.POOL_SIZE = 1;
        
        try {
            if (args.length > 0)
                jde.util.usages.ClassPathManager.POOL_SIZE = Integer.parseInt(args[0]);
        } catch (Exception e) {
            e.printStackTrace();
            jde.util.usages.ClassPathManager.POOL_SIZE = 1;
        }
        
        System.out.println ("jde.util.usages.ClassPathManager.POOL_SIZE = " + jde.util.usages.ClassPathManager.POOL_SIZE); 

        String className = "Usages|jde.util.";
//         if (args.length > 0)
//             className = args[0];
        findSubs(className, true, true, System.out);
    }
}
