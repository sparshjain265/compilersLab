class A {
	int a;
	public int b(int k)
	{
		return 1; 
	}
}


class test01 { 
	int c = 0;

	public boolean myFun(int a, boolean c)
	{
		if (a == 0)
			return c;
	}

	public int b()
	{
		return 1;
	}

	public void d()
	{
		return;
	}

	public static void main(String[] args) {
		A a;
		int[] bsharp = new int[3];
		int b;
		bsharp[0] = 100;
		
		
		if(b < 0)
			b = 5;

		a = new A();
		System.out.println("TEST01"); 
		System.out.println(123); 
		System.out.println(a.b(b)); 
		System.out.println(bsharp[0]);
		System.out.println(true); 
		System.out.println(c);
	} 
}

