public class A {
    int a5 = 1;
    int[] a = { 3,4,5 };
    boolean a2 = false;

    public void fooA(){
    
    int e3 = 8;
    int g1 = 6;
    int y1;
    e3 = 9;
}
    public void fooA1(int e1, int g2, boolean y2){
    e1 = 8;
    int g1 = 6;
    int y1;
}
// fooB will not pass
    public void fooB(){
    
    int e13 = 8;
    int q1 = 6;
    boolean q1 = true;
}
// fooC will not pass
    public void fooC(int e1, int e1, boolean y2){
    int e13 = 8;
    int q1 = 6;
}
// fooD will not pass
    public void fooD(int e13, int e1, boolean y2){
    int e13 = 8;
    int q1 = 6;
}

}

public class B {
    int a5 = 1;
    int[] a = { 3,4,5 };
    boolean a2 = false;

    public void fooA(){
    
    int e8 = 8;
    int g4 = 6;
    int y0;
}
// fooB will not pass
    public void fooB(){
    
    int e1 = 8;
    int e1;
}

}



