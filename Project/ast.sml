structure AST = struct

	type ID = string

	(* define basic data types *)
	datatype Basic = bool | int

	datatype Type	= basicType of Basic
					| arrayType of Basic
					| objType of ID
					| voidType
	
	(* define operators *)
	datatype Binop	= ADD | SUB | MUL | DIV (* arithmetic *)
					| AND | OR				(* boolean *)

	datatype Relop	= EQ | NE | LT | LE | GT | GE (* comparison *)

	(* expressions *)
	datatype Exp	= Const of Basic				(* 5, true *)
					| BINOP of Binop * Exp * Exp	(* 2 + 3 *)
					| RELOP of Relop * Exp * Exp	(* 2 < 3 *)
					| NOT of Exp					(* !x *)
					| ArrayElement of Exp * Exp		(* a[0] *)
					| ArrayLength of Exp			(* a.length() *)
					| Call of Exp * ID * Exp list	(* A.f(1,2) *)
					| NewArray of Basic * Exp 		(* new int[3] *)
					| NewObject of ID				(* new A() *)
					| THIS							(* this.something *)
					| Var of ID						(* variable *)
	
	(* statements *)
	datatype Stmt	= Block of Stmt list							(* {list of statements} *)
					| Assign of Exp option * ID * Exp option * Exp	(* A.x[0] = 2 *)
					| CallStmt of Exp * ID * Exp list				(* A.f(1,2) *)
					| IF of Exp * Stmt * Stmt						(* if exp then stmt1 else stmt 2 *)
					| While of Exp * Stmt							(* while exp stmt *)
					| PrintE of Exp 								(* print(e) *)
					| PrintS of string								(* print("abc") *)
					| Return of Exp option							(* return x+3 *)

	(* variable declarations *)
	datatype VarDec	= VarDec of Type * ID * Exp option	(* int x = 5 *)

	(* formal parameters of a method *)
	datatype Formal = Formal of Type * ID

	(* method declaration *)
	datatype MethodDec = MethodDec of Type * ID *	(* return type and method name *)
							Formal list * 			(* function parameters *)
							VarDec list *			(* declaration of local variables *)
							Stmt list				(* list of statements *)
	
	(* class declaration *)
	datatype ClassDec = ClassDec of ID * 			(* class name *)
							VarDec list * 			(* list of variable declarations *)
							MethodDec list			(* list of method declarations *)

	(* program is a list of class declarations *)
	datatype Program = Program of ClassDec list

end				