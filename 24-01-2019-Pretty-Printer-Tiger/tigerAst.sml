(* AST for Tiger Language *)

structure Ast = struct 

	datatype Program	= 	EXP of Exp
							|	DECS of Dec list
	
		and Exp	=	(* literals *)
							NIL
						|	INTEGER of int
						|	STRING of string
							(* Array and record creations *)

		(* and Decs	=	Dec list *)

		and Dec	=	string





	(* datatype Expr =	Const of int
						|	Var of string
						|	Op of Expr * BinOp * Expr

		and	BinOp =	Plus
						|	Minus
						|	Mul
						|	Div

	(* fun	binOpDenote Plus 	x y = x + y
		|	binOpDenote Minus x y = x - y
		|	binOpDenote Mul	x y = x * y
		|	binOpDenote Div	x y = x div y *)

	(* fun	exprDenote (Const x) 			= x
		|	exprDenote (Var x)				= x
		|	exprDenote (Op (x, oper, y))	= binOpDenote oper (exprDenote x) (exprDenote y) *)

	fun	binOpToString Plus	= "+"
		|	binOpToString Minus	= "-"
		|	binOpToString Mul		= "*"
		|	binOpToString Div		= "/"

	fun plus		a b	= Op (a, Plus, b)
	fun minus	a b	= Op (a, Minus, b)
	fun mul		a b	= Op (a, Mul, b)
	fun divide	a b	= Op (a, Div, b) *)

end