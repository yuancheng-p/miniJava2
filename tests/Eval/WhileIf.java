public class WhileIf {
  public static void main (String [] args) {
    int sum = 1, i = 1;
    // calculate 10!
    while (i <= 10) {
      sum = sum * i++;
    }

    int result = 0;
    if (sum > 5050) {
      result = sum;
    } else {
      result = 5050;
    }

    int r = result;

  }
}
