public class DynamicBindingTest {

  public static void main(String [] args) {
    A b = new B();
    b.foo();
    b = new C();
    b.foo();
    b = new A();
    b.foo();
  }
}

class A {
  public int foo () {
    return 100;
  }
}


class B extends A {
  public int foo () {
    return 200;
  }
}


class C extends A {
  public int foo () {
    return 300;
  }
}
