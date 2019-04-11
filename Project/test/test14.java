class MyClass {
  int n;

  public int factorial(int n)
  {
    int result = 1;
    while(n > 1)
    {
      result = result * n;
      n = n - 1;
    }
    return result;
  }

  public static void main(String[] args) {
    n = 10;
    System.out.println(factorial(n)); 
  }
}