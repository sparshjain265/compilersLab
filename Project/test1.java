// simple test for lex analysis
// Contains illegal character - line 4, # in ID
class test01 { 
	int c;

	public int myFun(int k, int n)
	{
		int a = k;
		return 0;
	}

	public static void main(String[] a) {
		int bsharp;
		int a;
		int b = ~1 + 0;
		b = 3 + 2;
		if(a<b)
			b = 5;
		while(true)
			myFun(a, b);
		System.out.println("TEST01"); 
		System.out.println(123); 
		System.out.println(); 
		System.out.println(true); 
	} 
}
