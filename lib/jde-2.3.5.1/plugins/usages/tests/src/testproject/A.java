package testproject;

public class A extends ASuper implements AInt {

    public int foo;
    
    public void method1 () { // overridden from Asuper
        foo = 1;
    }
    
    public void method2 () { // overridden from ASuperSuper
        
    }

    public void method3 () {// declared in AInt
        
    }

    public void method4 () {// only in A
        
    }

    public class B {
        public A methodB () {
            return A.this;
        }
    }

    public static class C extends A implements I, Comparable {

        public int methodI (B b) {
            b.methodB();
            return 0;
        }

        public int compareTo (Object o) {
            return 0;
        }
        
    }

    public interface I {

        public int methodI (A.B b);
    }

    public static class D {
        public String method1 () {
            return "D";
        }
    }
    
}