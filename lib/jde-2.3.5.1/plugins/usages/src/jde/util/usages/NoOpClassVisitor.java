package jde.util.usages;

import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Attribute;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.FieldVisitor;

/**
 * A ClassVisitor implementation that does nothing. This is used as a base class
 * to avoid repeating the empty method definitions.
 *
 **/
public class NoOpClassVisitor implements ClassVisitor {


    public void visit (int version, int access, String name, String signature,
                       String superName, String[] interfaces)    {}

    public void visitSource (String source, String debug) {}
    
    public void visitOuterClass (String owner, String name, String desc) {}

    public AnnotationVisitor visitAnnotation (String desc, boolean visible) {return NoOpAnnotationVisitor.SINGLETON;}
    
    public void visitAttribute (Attribute attr) {}
    
    public   void visitInnerClass (String name, String outerName, String innerName, int access) { }

    public FieldVisitor visitField(int n, String name, String desc, String sig, Object value) { return null; }

    public MethodVisitor visitMethod(int n, String name, String desc, String sig, String[] exceptions) {
        return null;
    }

    public void visitEnd() {
    
    }

    public static class NoOpAnnotationVisitor implements AnnotationVisitor  {

        public static NoOpAnnotationVisitor SINGLETON = new NoOpAnnotationVisitor ();
        
        public void visit (String name, Object value) {}
  
        public void visitEnum (String name, String desc, String value) {}
  
        public AnnotationVisitor visitAnnotation (String name, String desc) { return SINGLETON;}
  
        public AnnotationVisitor visitArray (String name) { return SINGLETON;}
  
        public void visitEnd () {}
    }
}
