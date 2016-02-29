public class MethodPolymorphysm {

  public static void main() {

    A a = new A ();
    int i = a.foo();
    int j = a.foo(99);

  }

}


class A {

  int foo() {
    return 100;
  }

  int foo(int i) {
    int k = i;
    return k;
  }
}
