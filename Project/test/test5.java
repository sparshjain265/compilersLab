class MyClass {
  int n;

  public static void main(String[] args) {
    n = 10;
    System.out.println(factorial(n)); 
  }

  public int factorial(int n)
  {
    if(n == 1) 
      return 1;
    else 
      return n*factorial(n-1);
  }
}