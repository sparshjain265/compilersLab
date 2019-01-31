(* AST for Tiger Language *)

structure Ast = struct 

	datatype Expr = Const of int
					|	Op of Expr * BinOp * Expr

		and	BinOp = Plus

	fun binOpDenote Plus x y = x + y;

	fun	exprDenote (Const x) 		= x
		|	exprDenote (Op (x, oper, y)) = binOpDenote oper (exprDenote x) (exprDenote y);

	fun	binOpToString Plus = "+"

	fun 	plus a b	= Op (a, Plus, b)

end