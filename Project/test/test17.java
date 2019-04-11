class Fact {
  int n;

  public int factorial(int n)
  {
    if(n == 0)
      return 1;
    else
    if(n == 1) 
      return 1;
    else 
      return n*factorial(n-1);
  }
}

class MyClass {
  Fact N;
  public static void main(String[] args) {
    N.n = 10;
    System.out.println(N.factorial(10)); 
  }
}