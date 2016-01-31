package tutu.titi.toto;

//class A can pass the test
public class A {
    Int a1 = 1;
    int[] a = { 3,4,5 };
    Bool a2 = false;
    String a6 = "jsdjkndjk\njdsdjkf";

    public void fooA();
    public void fooA(int toto);

}

//class B can't pass the test
public class B {
    int fooB(int tata);
    void fooB(int toto);
}
