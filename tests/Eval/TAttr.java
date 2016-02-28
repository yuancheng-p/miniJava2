public class Main {

  public static void main () {
    Person p = new Person();
    p.setAge(12);
    int a = p.getAge();
    int k = 1;
    Node n1 = new Node();
    n1.val = 888;
    n1.next = new Node();
    n1.next.val = 999;
    n1.next.next = new Node();
    n1.next.next.val = 111;
    n1.val = p.age;
  }
}


class Person {

  int age;

  void setAge(int a) {
    this.age = a;
  }

  int getAge() {
    return age;
  }

}

class Node {
  public int val;
  public Node next;
}
