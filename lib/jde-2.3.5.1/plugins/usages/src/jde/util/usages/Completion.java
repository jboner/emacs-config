package jde.util.usages;

import java.io.Writer;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.IOException;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.Opcodes;

import jde.util.Usages;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.Type;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.Attribute;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import java.util.Stack;

/**
 * This class provides completion facilities. It is largely copied from
 * jde.util.Completion written by Rodrigo Reyes (reyes@chez.com), and then
 * adapted to use jde-usages helper methods instead of the java reflection
 * classes.
 *
 */

public class Completion extends jde.util.Completion {
    /**
     * Tests whether a class is an ancestor of another class.
     * This method prints "t" to standard out if the class is an ancestor
     * of the other class. If the class is not an ancestor or either class
     * cannot be found, this method prints nil to standard out.
     * @param ancestor Name of the ancestor class
     * @param child  Name of the supposed child class
     */
    public static void isAncestorOf(String ancestor, String child) {

        ASMUsages asm = (ASMUsages) Usages.getCurrentIUsages();
        
        try {
            ClassReader classChild = asm.classpath.getClassReader (child);
            if (asm.classpath.isSubclass(ancestor, classChild)) {
                System.out.println(T);
            } else {
                System.out.println(NIL);
            }	  
        } catch (Exception ex) {
            System.out.println(NIL);
        }
    }
    
    /**
     * Returns true if the entity is accessible to the specified level of
     * protection.
     * @param access the access flags as passed in to the ClassVisitor.visit* methods
     * @param level the level of protection
     * @return if the member is accessible to the specified level of
     * protection.
     */
    private static boolean isAccessible(int access, int level) {
//         if ((access & Opcodes.ACC_SYNTHETIC) > 0)
//             return false;
        
        switch(level) {
        case PUBLIC:    // member is accessible if it has public access
            return  (access & Opcodes.ACC_PUBLIC) > 0;
        case PROTECTED: // accessible if member is protected
            return  (access & Opcodes.ACC_PROTECTED) > 0;
        case DEFAULT: // accessible if member is not public, protected
            // or private
            return
                ! ((access & (Opcodes.ACC_PUBLIC | Opcodes.ACC_PROTECTED | Opcodes.ACC_PRIVATE)) > 0);
          
        case PRIVATE:   // accessible if member is private
            return  (access & Opcodes.ACC_PRIVATE) > 0;
        default:
            // cannot get here any more, since the access level is
            // only used internally
            throw new Error("Completion.isAccessible(int, int) "
                            + "called with incorrect access level parameter:"
                            + level);
        }//switch
    }

    
    /**
     * Prints (list "name" "type") to the system output.
     *
     * @param name field name 
     * @param type field type
     */
    private static String printField(String name, String type) {
        StringBuffer sb = new StringBuffer (30);
        sb.append(START_LIST);
        sb.append(printWithinQuotes(name));
        sb.append(SPACE);
        sb.append(printWithinQuotes(Type.getType (type).getClassName ()));
        sb.append(END_PAREN);
        
        return sb.toString();
    }
    
    /**
     * Prints (list "name" "params") to the system output.
     *
     * @param name constructor name
     * @param params parameter type
     */
    private static String printConstructor(String name, String desc, String[] exceptions) {
        StringBuffer sb = new StringBuffer (30);
        Type[] args = Type.getArgumentTypes(desc);
        sb.append(START_LIST);
        sb.append(printWithinQuotes(name));
        sb.append(SPACE);
        sb.append(listClassArray(args));
        sb.append(SPACE);
        sb.append (printExceptions (exceptions));
        sb.append(END_PAREN);

        return sb.toString();
    }

    /**
     * Prints (list "name" "returnType" "args") to the system output.
     *
     * @param name method name
     * @param returnType method return type
     * @param args method arguments
     */
    private static String printMethod(String name, String desc, String[] exceptions) {
        StringBuffer sb = new StringBuffer (30);
        Type returnType = Type.getReturnType(desc);
        Type[] args = Type.getArgumentTypes(desc);
        sb.append(START_LIST);
        sb.append(printWithinQuotes(name));
        sb.append(SPACE);
        sb.append(printWithinQuotes(returnType.getClassName()));
        sb.append(SPACE);
        sb.append(listClassArray(args));
        sb.append(SPACE);
        sb.append (printExceptions (exceptions));
        sb.append(END_PAREN);
        
        return sb.toString(); 
    } 

    /**
     * Prints (list "className") to the system output.
     *
     * @param name className
     */
    private static String printClass(String name) {
        StringBuffer sb = new StringBuffer (30);
        sb.append(START_LIST);
        sb.append(printWithinQuotes(name));
        sb.append(END_PAREN);
        return sb.toString();
    }
    
    /**
     * <code>printExceptions</code>
     *
     * @param exceptions a <code>Class[]</code>
     * @return a <code>String</code>
     */
    private static String printExceptions(String[] exceptions) {
        if (exceptions == null)
            return NIL;
        
        StringBuffer sb = new StringBuffer (30);

        sb.append(START_LIST);
        for (int i = 0; i < exceptions.length; i++) {
            sb.append(printWithinQuotes(exceptions[i]));
            if ((i + 1) != exceptions.length) {
                sb.append(SPACE);
            }
        }
        sb.append(END_PAREN);

        return sb.toString();
    }
    
    /**
     * Prints item within quotes i.e "item"
     *
     * @param item string to be quoted.
     */
    private static String printWithinQuotes(String item) {
        StringBuffer sb = new StringBuffer (30);
        sb.append(DOUBLE_QUOTE);
        sb.append(item.replace('/', '.'));
        sb.append(DOUBLE_QUOTE);
        
        return sb.toString();
    }
 
    
    private static void listClassInfo(final String classNameFirst, final int level, StringBuffer sb) {

        final StringBuffer fields = new StringBuffer();
        final StringBuffer methods = new StringBuffer();
        final StringBuffer constructors = new StringBuffer();
        final StringBuffer innerClasses = new StringBuffer();
        final Stack classes = new Stack ();

        if (!isArray (classNameFirst))
            classes.push (classNameFirst);
        else {
            classes.push ("java.lang.Object");
            fields.append (printField ("length", "I"));
        }
        

        ASMUsages asm = (ASMUsages) Usages.getCurrentIUsages();
        
        while (classes.size() > 0) {
            final String className = (String) classes.pop();
            ClassReader cr = asm.classpath.getClassReader (asm.getRealName (className));

            if (cr == null)
                continue;
            
            cr.accept(
                      new ClassVisitor() {
                      
                          /**
                           * Visits the header of the class.
                           *
                           * @param version the class version.
                           * @param access the class's access flags (see {@link Opcodes}). This
                           *      parameter also indicates if the class is deprecated.
                           * @param name the internal name of the class (see
                           *      {@link Type#getInternalName() getInternalName}).
                           * @param signature the signature of this class. May be <tt>null</tt> if the
                           *      class is not a generic one, and does not extend or implement generic
                           *      classes or interfaces.
                           * @param superName the internal of name of the super class (see
                           *      {@link Type#getInternalName() getInternalName}). For interfaces, the
                           *      super class is {@link Object}. May be <tt>null</tt>, but only for the
                           *      {@link Object} class.
                           * @param interfaces the internal names of the class's interfaces (see
                           *      {@link Type#getInternalName() getInternalName}). May be <tt>null</tt>.
                           */

                          public void visit (
                                             int version,
                                             int access,
                                             String name,
                                             String signature,
                                             String superName,
                                             String[] interfaces) {
                              if (superName != null)
                                  classes.add (superName);
                              for (int i = 0 ; i < interfaces.length;i++)
                                  classes.add (interfaces[i]);
                          
                          }

                          public void visitSource (String source, String debug) {}

                          /**
                           * Visits the enclosing class of the class. This method must be called only
                           * if the class has an enclosing class.
                           *
                           * @param owner internal name of the enclosing class of the class.
                           * @param name the name of the method that contains the class, or
                           *      <tt>null</tt> if the class is not enclosed in a method of its
                           *      enclosing class.
                           * @param desc the descriptor of the method that contains the class, or
                           *      <tt>null</tt> if the class is not enclosed in a method of its
                           *      enclosing class.
                           */

                          public void visitOuterClass (String owner, String name, String desc) {}

                          public AnnotationVisitor visitAnnotation (String desc, boolean visible) { return null;} 

                          public void visitAttribute (Attribute attr) {}
                      
                          public void visitInnerClass (
                                                       String name,
                                                       String outerName,
                                                       String innerName,
                                                       int access) {
                              if (innerName != null && isAccessible (access, level))
                                  addIfNew (innerClasses, printClass (name));
                              
                          }

                          /**
                           * Visits a field of the class.
                           *
                           * @param access the field's access flags (see {@link Opcodes}). This
                           *      parameter also indicates if the field is synthetic and/or deprecated.
                           * @param name the field's name.
                           * @param desc the field's descriptor (see {@link Type Type}).
                           * @param signature the field's signature. May be <tt>null</tt> if the field's
                           *      type does not use generic types.
                           * @param value the field's initial value. This parameter, which may be
                           *      <tt>null</tt> if the field does not have an initial value, must be an
                           *      {@link Integer}, a {@link Float}, a {@link Long}, a {@link Double} or
                           *      a {@link String} (for <tt>int</tt>, <tt>float</tt>, <tt>long</tt>
                           *      or <tt>String</tt> fields respectively). <i>This parameter is only
                           *      used for static fields</i>. Its value is ignored for non static
                           *      fields, which must be initialized through bytecode instructions in
                           *      constructors or methods.
                           * @return a visitor to visit field annotations and attributes, or
                           *      <tt>null</tt> if this class visitor is not interested in visiting
                           *      these annotations and attributes.
                           */

                          public FieldVisitor visitField (
                                                          int access,
                                                          String name,
                                                          String desc,
                                                          String signature,
                                                          Object value) {
                              if (isAccessible (access, level))
                                  addIfNew (fields, printField (name, desc));
                              return null;
                          }

                          /**
                           * Visits a method of the class. This method <i>must</i> return a new
                           * {@link MethodVisitor} instance (or <tt>null</tt>) each time it
                           * is called, i.e., it should not return a previously returned visitor.
                           *
                           * @param access the method's access flags (see {@link Opcodes}). This
                           *      parameter also indicates if the method is synthetic and/or deprecated.
                           * @param name the method's name.
                           * @param desc the method's descriptor (see {@link Type Type}).
                           * @param signature the method's signature. May be <tt>null</tt> if the method
                           *      parameters, return type and exceptions do not use generic types.
                           * @param exceptions the internal names of the method's exception
                           *      classes (see {@link Type#getInternalName() getInternalName}). May be
                           *      <tt>null</tt>.
                           * @return an object to visit the byte code of the method, or <tt>null</tt> if
                           *      this class visitor is not interested in visiting the code of this
                           *      method.
                           */

                          public MethodVisitor visitMethod (
                                                            int access,
                                                            String name,
                                                            String desc,
                                                            String signature,
                                                            String[] exceptions) {
                              if (isAccessible (access, level)) {
                                  if (name.equals ("<init>")) {
                                      if (classNameFirst == className) {
                                          addIfNew (constructors, printConstructor (className, desc, exceptions));
                                      }
                                  } else {
                                      addIfNew (methods, printMethod (name, desc, exceptions));
                                  }
                                  
                              }
                              return null;                          
                          }

                          public void visitEnd () {}
                      },
                      true);
        }       
        sb.append(START_LIST);
        
        //we add the protected/private fields depending on the access level
        sb.append(START_LIST);
        sb.append (fields);
        sb.append(END_PAREN);

        // constructors
        sb.append(NL);
        sb.append(START_LIST);
        sb.append (constructors);
        sb.append(END_PAREN);

        // methods, added recursively
        sb.append(NL);
        sb.append(START_LIST);
        sb.append (methods);
        sb.append(END_PAREN);

        // inner classes, added recursively
        sb.append(NL);
        sb.append(START_LIST);
        sb.append (innerClasses);
        sb.append(END_PAREN);
        sb.append(END_PAREN);
        sb.append(NL);
    
        return;
    }
    /**
     * Gets information on the specified class. Information is returned as a 
     * list of lists that is printed to System.out.
     *
     * @param className a <code>String</code> value
     */
    public static void getClassInfo(String className) {
        getClassInfo(className, PUBLIC);
    }
    
    /**
     * Gets information on the specified class. Information is returned as a 
     * list of lists that is printed to System.out.
     *
     * @param className a <code>String</code> value
     * @param level access level i.e. public, protected, default, and private
     */
    public static void getClassInfo(String className, int level) {

        StringBuffer sb = new StringBuffer (3000);
        sb.append(START_LIST);
        sb.append(NL);
        listClassInfo(className, level, sb);
        sb.append(END_PAREN);
        sb.append(NL);
        
        Writer out
            = new BufferedWriter(new OutputStreamWriter(System.out));
        try {
            out.write(sb.toString());
            out.flush();
        } catch (IOException e) {
        }
    }

    private static boolean isArray (String className){
        return className.startsWith ("[");
    }
    
    /**
     * Looks up an unqualified class name in the class path to find possible
     * fully qualified matches.
     *
     * @param className a value of type 'String'
     * @param imports   an array of imported packages
     */
    public static void getClassInfo(String className,
                                    String[]imports) {
        String name; 
        ASMUsages asm = (ASMUsages) Usages.getCurrentIUsages();
        ClassReader cr;
        
        for (int i = 0 ; i < imports.length ; i++) {
            name = imports[i] + className;
            cr = asm.classpath.getClassReader (name);
            if (cr != null) {
                getClassInfo(name);
            }
        }
        System.out.println(NIL);
    }
    
    static String className(Class c) {
        if (c.isArray())
            return c.getComponentType().getName() + "[]";
        else
            return c.getName();
    }
    
    static String listClassArray(Type[] classes) {
        StringBuffer sb = new StringBuffer (100);
        for (int i = 0; i < classes.length; i++) {
            sb.append(printWithinQuotes(classes[i].getClassName()));
            if ((i + 1) != classes.length) {
                sb.append(SPACE);
            }
        }
        return sb.toString();
    }

    static void addIfNew (StringBuffer sb, String signature) {
        // JDEE uses lastIndexOf, which doesn't work in JDK 1.3
        // TODO: is there any reason to use lastIndexOf instead of indexOf in later JDKs?
        if (!System.getProperty("java.version").startsWith ("1.3")) {
            if (sb.indexOf (signature) == -1)
                sb.append (signature);
        } else {
            if (sb.lastIndexOf (signature) == -1)
                sb.append (signature);
        }
    }
    
    public static void main (String[] args)  {

        String className = "java.lang.Object";
      
        if (args.length > 0 )
            className = args[0];
      
        try {
            Class.forName(className);
      
            getClassInfo(className, 0);
            getClassInfo(className, 1);
            getClassInfo(className, 2);
            getClassInfo(className, 3);
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
      
    }

}