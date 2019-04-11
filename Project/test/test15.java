class MyClass {
  int n;

  public int factorial(int n)
  {
    if(n)
      return 1;
    else
    if(n == 1) 
      return 1;
    else 
      return n*factorial(n-1);
    

  }

  public static void main(String[] args) {
    n = 10;
    while(n)
      System.out.println(factorial(n)); 
  }
}