public class TestEval {

  public static void main(String [] args) {
    double a = 1.2;
    double c = 1.2 + a;

    // test TNew
    TestEval te = new TestEval();

    // test TCall & TReturn
    (new TestEval()).add(te.bar(), 122);

    // test TBlock
    boolean b = true;
    {
      b = false;
      boolean v2 = true;
    }

    // test if else
    if (a > 0 && false) {
      c = a;
      int temp = 110;
    } else {
      c = a + 2;
    }

    te.test_while();
  }

  public void test_while() {
    int i = 0;
    while (i < 2) {
      i = i + 100;
    }
  }

  public int bar () {
    return 888;
  }


  public int add(int a, int b) {
    return a + b;
  }

  public void foo () {
    int f = 999;
    return;
  }
}


class Node {
  public void foobar() {
  }
}
