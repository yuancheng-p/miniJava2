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

  public int foo () {
    int i = 0;
    if (i > 0)
      return 1;
    else
      return 0;
  }

  public int fo () {
    return 1;
  }
  public void fo1 () {
    return;
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
