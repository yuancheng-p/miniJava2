public class Inheritance {

  public static void main (String [] args) {

    B b = new B();
    int i = b.getNumberA();
    int j = b.getNumberB();
  }
}

class A {
  int a = 10;
  int getNumberA () {
    return a;
  }
}


class B extends A {
  int k = 20;
  int getNumberB () {
    return k;
  }
}

