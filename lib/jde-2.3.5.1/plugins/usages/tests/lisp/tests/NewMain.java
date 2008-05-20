package testproject;

public class Main {
    public Main() {
        
    }

    public void test () {
        AClass a = null;
        ASuper as = null;
        ASuperSuper ass = null;
        AInt ai = null;
        ASuperInt asi = null;
        as = a;
        ass = a;
        ai = a;
        asi = a;
        a.method1();
        a.method2();
        a.method3();
        a.method4();
        as.method1();
        as.method2();
        ass.method2();
        ai.method1();
    }
    
} // Main
