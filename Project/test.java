class ABC {
  int x = 10;

  public int get_x()
  {
    return x;
  }
}

class MyClass {
  int x = 5;

  public int get_x ()
  {
    return x;
  }

  public static void main(String[] args) {
    ABC myObj1 = new ABC();  // Object 1
    ABC myObj2 = new ABC();  // Object 2
    System.out.println(myObj1.get_x());
    System.out.println(myObj2.get_x());
  }
}