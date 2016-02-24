class I {}
class A extends I{
    int a1 = 1;
    String a6 = "jsdjkndjk\njdsdjkf";
}

abstract class B {
    int a5 = 1;
    boolean a2 = false;
    public int fooA(){
    int i = 1;
    int y = 2;
    return 1+1;
}

    public A fooA1(){
    int i = 1;
    int y = 2;
    return new A();
}

    public I fooA2(){
    int i = 1;
    int y = 2;
    return new A();
}
// fooB will not pass
    public void fooB(){
    int i = 1;
    int y = 0;
    return 1+1;
}

// fooB1 will not pass
    public A fooB1(){
    int i = 1;
    int y = 0;
    return new I();
}
}



