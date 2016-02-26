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
