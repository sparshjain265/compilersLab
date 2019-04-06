structure codeGen = struct
open AST
(* helpers *)

val oStream = ref (TextIO.openOut "a.js")
fun fprint outstream str = TextIO.output(outstream, str)

open printColor
val indent = ref 0
fun more () = (indent := (!indent) + 1)
fun less () = (indent := (!indent) - 1)
val n = ref 0
fun print_t () = if !n = 0 then ( fprint (!oStream) ("")) else (n := !n - 1 ; fprint (!oStream)  ("\t") ; print_t ())
fun printTabs () = (n := !indent ; print_t () )

val isError = ref false;
val level = ref 0;
fun nextLevel () = (level := (!level) + 1)
fun prevLevel () = (level := (!level) - 1)
val variableList : (ID * Type * int) list ref = ref []
val functionList : (ID * Type * int) list ref = ref []

val mainClass = ref ""
val isMain = ref false
val retType : Type option ref = ref NONE
val isReturned = ref false

structure funKey : ORD_KEY = 
struct
	type ord_key = Atom.atom * Atom.atom

	fun compare ((x1, y1), (x2, y2)) = case Atom.compare(x1, x2) of
											LESS => LESS
										|	GREATER => GREATER
										|	EQUAL => Atom.compare(y1, y2)
end

structure classFunMap = RedBlackMapFn(funKey)

val functionMap : (Type list) classFunMap.map ref = ref classFunMap.empty

val classList : ID list ref = ref []
val classMap : ((ID * Type * int) list) AtomMap.map ref = 
	ref (
			AtomMap.insert
			(
				AtomMap.singleton(Atom.atom "this", []), 
				Atom.atom "String", 
				[]
			)
		)

fun deleteFunLevel L = 
	if List.null (!functionList) then
		[]
	else
		let
			val (id, t, l) = List.hd (!functionList)
			val v = (functionList := List.tl (!functionList))
		in
			if (l < L) then ((id, t, l) :: deleteFunLevel L) else (deleteFunLevel L)
		end

fun deleteLevel L = 
	if List.null (!variableList) then
		[]
	else
		let
			val (id, t, l) = List.hd (!variableList)
			val v = (variableList := List.tl (!variableList))
		in
			if (l < L) then ((id, t, l) :: deleteLevel L) else (deleteLevel L)
		end

fun levelUp () = nextLevel ()
fun levelDown () = 
	let
		val l = !level
		val v = (variableList := (deleteLevel l))
		val f = (functionList := (deleteFunLevel l))
	in
		prevLevel ()
	end

(* Printing *)

(* fun printBasic Bool = (fprint (!oStream)  "bool"; basicType Bool)
|	printBasic Int	= (fprint (!oStream)  "int"; basicType Int)

fun printType (basicType b) = (printBasic b; basicType b)
|	printType (arrayType b) = (printBasic b; fprint (!oStream)  "[]"; arrayType b)
|	printType (objType id)	= 
	let
		val t = (fprint (!oStream)  id; objType id)
	in
		if AtomMap.inDomain(!classMap, Atom.atom id) then 
			t 
		else 
			(isError := true; print_red " Unknown Type! "; t)
	end
|	printType voidType		= (fprint (!oStream)  "void"; voidType) *)

fun printType x = 
	if !level = 1 then
		(fprint (!oStream)  ""; x)
	else
		(fprint (!oStream)  "var"; x)

fun printBinop ADD = fprint (!oStream)  "+"
|	printBinop SUB = fprint (!oStream)  "-"
|	printBinop MUL = fprint (!oStream)  "*"
|	printBinop DIV = fprint (!oStream)  "/"
|	printBinop AND = fprint (!oStream)  "&&"
|	printBinop OR  = fprint (!oStream)  "||"

fun printRelop EQ = fprint (!oStream)  "=="
|	printRelop NE = fprint (!oStream)  "!="
|	printRelop LT = fprint (!oStream)  "<"
|	printRelop LE = fprint (!oStream)  "<="
|	printRelop GT = fprint (!oStream)  ">"
|	printRelop GE = fprint (!oStream)  ">="

fun printConstant (Cint x) = (fprint (!oStream) (Int.toString x); basicType Int)
|	printConstant (Cbool x) = (fprint (!oStream) (Bool.toString x); basicType Bool)

fun printExp (Const c) 				= printConstant c
|	printExp (BINOP(bop, e1, e2)) 	= 
	let
		val t1 = printExp e1
		val p1 = fprint (!oStream)  " "
		val p2 = printBinop bop
		val p3 = fprint (!oStream)  " "
		val t2 = printExp e2
	in
		if t1 = t2 then
			if (bop = ADD orelse bop = SUB orelse bop = MUL orelse bop = DIV) then
				if t1 = (basicType Int) then
					t1
				else
					(
						isError := true; 
						print_red "Illegal Expression! Operation only allowed on integers"; 
						basicType Int
					)
			else 
				if t1 = (basicType Bool) then
					t1
				else
					(
						isError := true; 
						print_red "Illegal Expression! Operation only allowed on bools"; 
						basicType Bool
					)
		else
			(
				isError := true; 
				print_red "Type mismatch! Both operands should be of same type"; 
				basicType Int
			)
	end
|	printExp (RELOP(rop, e1, e2)) 	= 
		let
			val t1 = printExp e1
			val p1 = fprint (!oStream)  " "
			val p2 = printRelop rop
			val p3 = fprint (!oStream)  " "
			val t2 = printExp e2
		in
			if t1 = t2 then
				if t1 = (basicType Int) then
					basicType Bool
				else
					(
						isError := true; 
						print_red "Illegal Expression! Binary Comparisions only allowed on integers"; 
						basicType Bool
					)
			else
				(
					isError := true; 
					print_red "Type mismatch! Both operands should be integers"; 
					basicType Bool
				)
		end
|	printExp (NOT(e)) 				= 
		let
			val t = (fprint (!oStream) "!"; printExp e)
		in
			if t = (basicType Bool) then
				t
			else
				(
					isError := true; 
					print_red "Type mismatch! Logical Not can only be applied on booleans"; 
					basicType Bool
				)
		end
|	printExp (ArrayElement(e1, e2))	= 
		let
			fun g (arrayType t) = t
			|	g _				= (
									isError := true; 
									print_red "Attempted to access element of a non array type!";
									Int
								)
			val t1 = g (printExp e1)
			val p1 = fprint (!oStream)  "["
			val t2 = printExp e2
			val p2 = fprint (!oStream)  "]"
		in
			if t2 = (basicType Int) then
				basicType t1
			else
				(
					isError := true; 
					print_red "Array Indices should be integers"; 
					basicType t1
				)					
		end 
|	printExp (ArrayLength(e)) 		= 
		let
			fun g (arrayType t) = arrayType t
			|	g _				= (
									isError := true; 
									print_red "Attempted to find length of non Array type!"; 
									voidType
								)
			val t = g (printExp e)
			val p = fprint (!oStream) ".length"
		in
			basicType Int
		end 
|	printExp (Call(e, id, eList)) 	=
		let
			fun g (objType ob) = ob
			|	g _	= (
						isError := true; 
						print_red "Attempted to call a function from a non-object!"; 
						"this"
					)
			val t1 = g (printExp e)
			val tList = (fprint (!oStream)  "."; fprint (!oStream)  id; fprint (!oStream)  "("; printExps eList)
			val p1 = fprint (!oStream)  ")"
			fun f (a, _, _) = (a = id)
			val funList = 
				if t1 = "this" then 
					SOME (!functionList) 
				else 
					AtomMap.find(!classMap, Atom.atom t1)
			val t = 
				if funList = NONE then 
					NONE 
				else 
					List.find f (valOf funList)
		in
			if t = NONE then
				(
					isError := true; 
					print_red "Attempted to call a non - defined function"; 
					voidType
				)
			else
				let
					val (a, b, c) = valOf t
					val fList = classFunMap.lookup(!functionMap, (Atom.atom t1, Atom.atom id))
				in
					if tList = fList then 
						b 
					else
						(isError := true; print_red "Formal Arguments don't match"; b)
				end
		end 
|	printExp (NewArray(b, e)) 		= (fprint (!oStream)  "[";fprint (!oStream)  "]"; arrayType b)
|	printExp (NewObject(id)) 		= (fprint (!oStream)  "new "; fprint (!oStream)  id; fprint (!oStream) "()"; objType id)
|	printExp (Member(e, id)) 		= 
		let
			val t = (printExp e; fprint (!oStream)  "."; printExp (Var(id)))
		in
			if e = This then
				(t)
			else
				(isError := true; print_red "Illegal attempt to access class member!"; t)
		end
|	printExp This					= (fprint (!oStream)  "this"; objType "this")
|	printExp (Var(id)) 				= 
		let
			(* val p = fprint (!oStream)  id *)
			fun f (a, _, _) = (a = id)
			val t = List.find f (!variableList)
		in
			if t = NONE then
				(isError := true; print_red "Undefined variable used!"; voidType)
			else
				let
					val (a, b, c) = valOf t
				in 
					if c = 2 then
						(fprint (!oStream)  a; b)
					else
						(fprint (!oStream)  "this."; fprint (!oStream)  a; b)
				end
		end 

and printExps [] 					= []
|	printExps [e] 					= [printExp e]
|	printExps (x :: xs)				= 
		let
			val t = printExp x
			val p = fprint (!oStream)  ", "
			val ts = printExps xs
		in
			(t::ts)
		end

fun printStatement (Block xlist) = 
	(
		printTabs();
		fprint (!oStream)  "{\n";
		more();
		map printStatement xlist;
		less();
		printTabs();
		fprint (!oStream)  "}\n"
	)
|	printStatement (Assign(e1, id, e2, e)) = 
		let
			val p1 = printTabs()
			val t1 = 
				(		
					let
						val ex1 = if (isSome(e1)) then (Member((valOf e1), id)) else (Var id)
						val ex2 = if (isSome(e2)) then ArrayElement(ex1, (valOf e2)) else ex1
					in
						printExp ex2
					end
				)
			val p2 = fprint (!oStream)  " = "
			val t2 = printExp e
			val p3 = fprint (!oStream)  ";"
		in
			if (t1 = t2) then 
				(fprint (!oStream)  "\n") 
			else 
				(isError := true; print_red "Type Mismatch in assignment!\n")
		end
|	printStatement (CallStmt(e, id, elist)) = 
		(
			printTabs();
			printExp (Call(e, id, elist));
			fprint (!oStream)  ";\n"
		)
|	printStatement (If(e, s1, Block [])) = 
		let
			val p1 = (printTabs(); fprint (!oStream)  "if(")
			val t = printExp e
			fun f (Block _) = printStatement s1
			|	f _			= (more(); printStatement s1; less())
			val p2 = (fprint (!oStream)  ") then\n";	f s1)
		in
			if (t = basicType Bool) then () 
			else (isError := true; print_red "Condition not of bool type!\n")
		end
|	printStatement (If(e, s1, s2)) = 
		let
			val p1 = (printTabs(); fprint (!oStream)  "if(")
			val t1 = printExp e
			fun f (Block x) = printStatement (Block x)
			|	f s			= (more(); printStatement s; less())
			val p3 = 
			(
				fprint (!oStream)  ") then\n";
				f s1;
				printTabs();
				fprint (!oStream)  "else";
				f s2
			)
		in
			if (t1 = basicType Bool) then () 
			else (isError := true; print_red "Condition not of bool type!\n")
		end
|	printStatement (While(e, s)) = 
		let
			val p1 = (printTabs(); fprint (!oStream)  "while(")
			val t1 = printExp e
			fun f (Block _) = printStatement s
			|	f _			= (more(); printStatement s; less())
			val p2 = 
			(
				fprint (!oStream)  ")\n";
				f s
			)
		in
			if (t1 = basicType Bool) then () 
			else (isError := true; print_red "Condition not of bool type!\n")
		end
|	printStatement (PrintE e) = 
		(
			printTabs();
			fprint (!oStream)  "document.write((";
			printExp e;
			fprint (!oStream)  ") + \"<br>\");\n"
		)
|	printStatement (PrintS str) = 
		(
			printTabs();
			fprint (!oStream)  "document.write(\"";
			fprint (!oStream)  str;
			fprint (!oStream)  "\" + \"<br>\");\n"
		)
|	printStatement (Return e) = 
		let
			val p1 = (printTabs(); fprint (!oStream)  "return")
			val t1 = if(isSome(e)) then (fprint (!oStream)  " "; printExp(valOf e)) else (voidType);
			val p2 = fprint (!oStream)  ";"
		in
			if t1 = valOf (!retType) then 
				(fprint (!oStream)  "\n"; isReturned := true) 
			else
				(isError := true; print_red "Doesn't match with function return type!\n")
		end

fun printVarDec (VarDec(typ, id, exp)) = 
	let
		val p1 = printTabs()
		val t1 = printType typ
		val p2 = (fprint (!oStream)  " "; fprint (!oStream)  id)
		val t2 = if(isSome(exp)) then (fprint (!oStream)  " = "; printExp(valOf exp)) else (typ)
		val p3 = fprint (!oStream)  ";"
		fun f (x, _, l) = x = id andalso l = !level
		val v1 =
			if List.exists f (!variableList) then
				(isError := true; print_red "Redeclaration of variable!\n")
			else
				(variableList := (id, typ, (!level)) :: (!variableList))
	in
		if(t1 = t2) then (fprint (!oStream)  "\n") else (isError := true; print_red "Type Mismatch in assignment!\n")
	end

fun printFormal (Formal(typ, id)) = 
	let
		val p1 = (fprint (!oStream)  id)
		fun f (x, _, l) = x = id andalso l = !level
	in
		if List.exists f (!variableList) then
			(isError := true; print_red "Using same name for different arguments!")
		else
			(variableList := (id, typ, (!level)) :: (!variableList))
	end

fun mapFormal className id (Formal(typ, _)) = 
	let
		val temp = 	if classFunMap.inDomain(!functionMap, (Atom.atom className, Atom.atom id)) then 
						classFunMap.lookup(!functionMap, (Atom.atom className, Atom.atom id)) 
					else []
	in
		(functionMap := classFunMap.insert(!functionMap, (Atom.atom className, Atom.atom id), typ :: temp);
		functionMap := classFunMap.insert(!functionMap, (Atom.atom "this", Atom.atom id), classFunMap.lookup(!functionMap, (Atom.atom className, Atom.atom id))))
	end

fun printFormals className id [] 		= (functionMap := classFunMap.insert(!functionMap, (Atom.atom className, Atom.atom id), []); 
											functionMap := classFunMap.insert(!functionMap, (Atom.atom "this", Atom.atom id), []))
|	printFormals className id [x] 		= ((printFormal x); mapFormal className id x)
|	printFormals className id (x::xs)	= (printFormal x; mapFormal className id x; fprint (!oStream)  ", "; printFormals className id xs)

fun printMethodDec className (MethodDec(typ, "main", flist, vlist, slist)) = 
	(
		if !isMain then 
			(isError := true; printTabs(); print_red "Exactly one main function is allowed!\n") 
		else 
			(isMain := true; mainClass := className);
		retType := SOME typ;
		isReturned := false;
		if typ = voidType then (isReturned := true) else ();
		printTabs();
		fprint (!oStream)  "main(";
		let
			val id = "main"
			fun f (x, _, l) = x = id andalso l = !level
		in 
			if List.exists f (!variableList) then
				(isError := true; print_red "Cannot use a declared variable name as a function name!")
			else
				if List.exists f (!functionList) then
					(isError := true; print_red "Function Overloading not allowed!")
				else
					(functionList := (id, typ, (!level)) :: (!functionList))
		end;
		levelUp ();
		printFormals className "main" flist;
		fprint (!oStream)  ")\n";
		printTabs();
		fprint (!oStream)  "{\n";
		more();
		map printVarDec vlist;
		map printStatement slist;
		less();
		printTabs();
		fprint (!oStream)  "}";
		if (!isReturned) then (fprint (!oStream)  "\n") else (isError := true; print_red "Function not returning!\n");
		retType := NONE;
		levelDown ()
	)
|	printMethodDec className (MethodDec(typ, id, flist, vlist, slist)) = 
	(
		retType := SOME typ;
		isReturned := false;
		if typ = voidType then (isReturned := true) else ();
		printTabs();
		(* fprint (!oStream)  "public "; *)
		(* printType typ; *)
		(* fprint (!oStream)  " "; *)
		fprint (!oStream)  id;
		let
			fun f (x, _, l) = x = id andalso l = !level
		in 
			if List.exists f (!variableList) then
				(isError := true; print_red "Cannot use a declared variable name as a function name!")
			else
				if List.exists f (!functionList) then
					(isError := true; print_red "Function Overloading not allowed!")
				else
					(functionList := (id, typ, (!level)) :: (!functionList))
		end;
		fprint (!oStream)  "(";
		levelUp();
		printFormals className id flist;
		fprint (!oStream)  ")\n";
		printTabs();
		fprint (!oStream)  "{\n";
		more();
		map printVarDec vlist;
		map printStatement slist;
		less();
		printTabs();
		fprint (!oStream)  "}";
		if (!isReturned) then (fprint (!oStream)  "\n") else (isError := true; print_red "Function not returning!\n");
		retType := NONE;
		levelDown()
	)	

fun printMethodDecs className [] = ()
|	printMethodDecs className (x :: xs) = 
	let
		val t1 = printMethodDec className x
		val t2 = printMethodDecs className xs
		val MethodDec(typ, id, _, _, _) = x
		val funList = AtomMap.lookup(!classMap, Atom.atom className)
	in
		(classMap := AtomMap.insert(!classMap, Atom.atom className, (id, typ, 0) :: funList))
	end

fun printClassDec (ClassDec (id, varDecList, methodDecList)) = 
	(
		printTabs();
		fprint (!oStream)  "class ";
		(classList := id :: (!classList));
		(classMap := AtomMap.insert(!classMap, Atom.atom id, []));
		fprint (!oStream)  id;
		fprint (!oStream)  "\n";
		printTabs();
		fprint (!oStream)  "{\n";
		more();
		levelUp();
		map printVarDec varDecList;
		printMethodDecs id methodDecList;
		less();
		printTabs();
		fprint (!oStream)  "}\n";
		levelDown()
	)



fun generate os (Program x) = 
	let
		val t = (TextIO.closeOut (!oStream); oStream := os)
	in
		(
			map printClassDec x;
			fprint (!oStream)  "var main = new ";
			fprint (!oStream)  (!mainClass);
			fprint (!oStream)  "();\n";
			fprint (!oStream)  "main = main.main();\n"
		)
	end

end