package tutu.titi.toto;


public class A {
//method A can pass the test
    public void fooA(){
    int a2=0, a3=0;
    if(1>0)
      a2 = 2;
    else
      a3 =7;
    boolean a4=false;
    if(2>1)
      a4= true;
    int a=0;
    int c = 0;
    while(a<10){
      a = a+1;
      while(c<10){
        c=c+1;
        int b=0;
        b = b+2;
      }
      int b = 8;
      b = b-4;
    }
    int b = 0;
    }
//method B can pass the test
    public void fooB(){
    
    if(1>0){
      int a2 = 2;
      int b2 =7;
      }
    else{
      int a3 =7;
      int b2 =7;}
    
    if(2>1){
      boolean a4 = true;}
    
    }
//method C can pass the test
    public void fooC(){
    int a2=0, a3=0;
    if(1>0)
      a2 = 2;
    else
      a3 =7;
    boolean a4=false;
    if(2>1)
      a4= true;
    int a=0;
    while(a<10){
      a = a+1;
      int a2 = 8;
      while(c<10){
        c=c+1;
        int b=0;
        b = b+2;
        int a3 = 9;
      }
      int b = 8;
      b = b-4;
    }
    }
    
    public static void main(String[] args){
      A t= new A();
      t.fooA();
      t.fooB();
    }

}
