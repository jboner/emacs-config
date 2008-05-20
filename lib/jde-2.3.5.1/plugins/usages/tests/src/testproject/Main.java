package testproject;









public class Main {
    public Main() {
        
    }

    public void test () {
        A a = null;
        ASuper as = null;
        ASuperSuper ass = null;
        AInt ai = null;
        ASuperInt asi = null;
        as = a;
        ass = a;
        ai = a;
        asi = a;
        a.method1();/*a.method1();*/a.method1();System.out.println ("a.method1();");new A.D().method1();a.method1();a.method1();
        a.method2();
        a.method3();
        a.method4();
        as.method1();
        as.method2();as.method2();as.method2();as.method2();
        ass.method2();
        ai.method1();
        A.I i = null;
        A.C c = null;
        A.B b = null;
        a = c;
        i = c;
        i.methodI (b);
        c.methodI (b);
        a = b.methodB();
        a.method5();
        as.method5();
        new Object().toString();
        new StringBuffer().toString();
    }
    
} // Main
