structure Semantic = struct

(* helpers *)
open printColor
val indent = ref 0
fun more () = (indent := (!indent) + 1)
fun less () = (indent := (!indent) - 1)
val n = ref 0
fun print_t () = if !n = 0 then ( print("")) else (n := !n - 1 ; print ("\t") ; print_t ())
fun printTabs () = (n := !indent ; print_t () )

(* Printing *)
open AST

val level = ref 0;
fun nextLevel () = (level := (!level) + 1)
fun prevLevel () = (level := (!level) - 1)
val variableList : (ID * Type * int) list ref = ref []
val functionList : (ID * Type * int) list ref = ref []
val functionMap : (Type list) AtomMap.map ref = ref AtomMap.empty

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


fun printBasic Bool = (print "bool"; basicType Bool)
|	printBasic Int	= (print "int"; basicType Int)

fun printType (basicType b) = (printBasic b; basicType b)
|	printType (arrayType b) = (printBasic b; print "[]"; arrayType b)
|	printType (objType id)	= (print id; objType id)
|	printType voidType		= (print "void"; voidType)

fun printBinop ADD = print "+"
|	printBinop SUB = print "-"
|	printBinop MUL = print "*"
|	printBinop DIV = print "/"
|	printBinop AND = print "&&"
|	printBinop OR  = print "||"

fun printRelop EQ = print "=="
|	printRelop NE = print "!="
|	printRelop LT = print "<"
|	printRelop LE = print "<="
|	printRelop GT = print ">"
|	printRelop GE = print ">="

fun printConstant (Cint x) = (print(Int.toString x); basicType Int)
|	printConstant (Cbool x) = (print(Bool.toString x); basicType Bool)

fun printExp (Const c) 				= printConstant c
|	printExp (BINOP(bop, e1, e2)) 	= 
	let
		val t1 = printExp e1
		val p1 = print " "
		val p2 = printBinop bop
		val p3 = print " "
		val t2 = printExp e2
	in
		if t1 = t2 then
			if t1 = (basicType Int) then
				t1
			else
				(print_red "Illegal Expression! Binary Operations only allowed on integers"; basicType Int)
		else
			(print_red "Type mismatch! Both operands should be integers"; basicType Int)
	end
|	printExp (RELOP(rop, e1, e2)) 	= 
		let
			val t1 = printExp e1
			val p1 = print " "
			val p2 = printRelop rop
			val p3 = print " "
			val t2 = printExp e2
		in
			if t1 = t2 then
				if t1 = (basicType Int) then
					basicType Bool
				else
					(print_red "Illegal Expression! Binary Comparisions only allowed on integers"; basicType Bool)
			else
				(print_red "Type mismatch! Both operands should be integers"; basicType Bool)
		end
|	printExp (NOT(e)) 				= 
		let
			val t = (print"!"; printExp e)
		in
			if t = (basicType Bool) then
				t
			else
				(print_red "Type mismatch! Logical Not can only be applied on booleans"; basicType Bool)
		end
|	printExp (ArrayElement(e1, e2))	= 
		let
			fun g (arrayType t) = t
			|	g _				= (print_red "Attempted to access element of a non array type!";Int)
			val t1 = g (printExp e1)
			val p1 = print "["
			val t2 = printExp e2
			val p2 = print "]"
		in
			if t2 = (basicType Int) then
				basicType t1
			else
				(print_red "Array Indices should be integers"; basicType t1)					
		end 
|	printExp (ArrayLength(e)) 		= 
		let
			fun g (arrayType t) = arrayType t
			|	g _				= (print_red "Attempted to find length of non Array type!"; voidType)
			val t = g (printExp e)
			val p = print".length"
		in
			basicType Int
		end 
|	printExp (Call(e, id, eList)) 	=
		let
			fun g (objType t) = objType t
			|	g _	= (print_red "Attempted to call a function from a non-object!"; voidType)
			val t1 = g (printExp e)
			val tList = (print "."; print id; print "("; printExps eList)
			val p1 = print ")"
			fun f (a, _, _) = (a = id)
			val t = List.find f (!functionList)
		in
			if t = NONE then
				(print_red "Attempted to call a non - defined function"; voidType)
			else
				let
					val (a, b, c) = valOf t
					val fList = AtomMap.lookup(!functionMap, Atom.atom id)
				in
					if tList = fList then b else (print_red "Types of Formal Arguments don't match"; b)
				end
		end 
|	printExp (NewArray(b, e)) 		= (print "new "; printBasic b; print "["; printExp e; print "]"; arrayType b)
|	printExp (NewObject(id)) 		= (print "new "; print id; print"()"; objType id)
|	printExp (Member(e, id)) 		= (printExp e; print "."; printExp (Var(id)))
|	printExp This					= (print "this"; objType "this")
|	printExp (Var(id)) 				= 
		let
			val p = print id
			fun f (a, _, _) = (a = id)
			val t = List.find f (!variableList)
		in
			if t = NONE then
				(print_red "Undefined variable used!"; voidType)
			else
				let
					val (a, b, c) = valOf t
				in 
					b
				end
		end 

and printExps [] 					= []
|	printExps [e] 					= [printExp e]
|	printExps (x :: xs)				= 
		let
			val t = printExp x
			val p = print ", "
			val ts = printExps xs
		in
			(t::ts)
		end

fun printStatement (Block xlist) = 
	(
		printTabs();
		print "{\n";
		more();
		map printStatement xlist;
		less();
		printTabs;
		print "}\n"
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

		(* if(isSome(e1)) then (printExp(valOf e1); print ".") else ();
		print id;
		if(isSome(e2)) then (print "["; printExp(valOf e2); print "]") else (); *)

		val p2 = print " = "
		val t2 = printExp e
		val p3 = print ";"
	in
		if (t1 = t2) then (print "\n") else (print_red "Type Mismatch in assignment!\n")
	end
|	printStatement (CallStmt(e, id, elist)) = 
	let
		val p1 = printTabs()
		val p2 = (printExp e; print "."; print id)
		val p2 = print "("
		val tList = printExps elist
		val p3 = print ");\n"
		val fList = 
			if (AtomMap.find(!functionMap, Atom.atom id) = NONE) then 
				(print_red "Attempted to Call a non-defined function!\n"; []) 
			else 
				(valOf (AtomMap.find(!functionMap, Atom.atom id)))
	in
		if tList = fList then () else (print_red "Types of Formal Arguments Don't Match!\n")
	end
|	printStatement (If(e, s1, Block [])) = 
	let
		val p1 = (printTabs(); print "if(")
		val t = printExp e
		val p2 = (print ") then\n";	more();	printStatement s1; less())
	in
		if (t = basicType Bool) then () else (print_red "Condition not of bool type!\n")
	end
|	printStatement (If(e, s1, s2)) = 
	let
		val p1 = (printTabs(); print "if(")
		val t1 = printExp e
		val p3 = 
		(
			print ") then\n";
			more();
			printStatement s1;
			less();
			printTabs();
			print "else";
			more();
			printStatement s2;
			less()
		)
	in
		if (t1 = basicType Bool) then () else (print_red "Condition not of bool type!\n")
	end
|	printStatement (While(e, s)) = 
	let
		val p1 = (printTabs(); print "while(")
		val t1 = printExp e
		val p2 = 
		(
			print ")\n";
			more();
			printStatement s;
			less()
		)
	in
		if (t1 = basicType Bool) then () else (print_red "Condition not of bool type!\n")
	end
|	printStatement (PrintE e) = 
	(
		printTabs();
		print "System.out.println(";
		printExp e;
		print ");\n"
	)
|	printStatement (PrintS str) = 
	(
		printTabs();
		print "System.out.println(\"";
		print str;
		print "\");\n"
	)
|	printStatement (Return e) = 
	(
		printTabs();
		print "return";
		if(isSome(e)) then (print " "; printExp(valOf e)) else (voidType);
		print ";\n"
	)

fun printVarDec (VarDec(typ, id, exp)) = 
	let
		val p1 = printTabs()
		val t1 = printType typ
		val p2 = (print " "; print id)
		val t2 = if(isSome(exp)) then (print " = "; printExp(valOf exp)) else (typ)
		val p3 = print ";"
		val v1 = (variableList := (id, typ, (!level)) :: (!variableList))
	in
		if(t1 = t2) then (print "\n") else (print_red "Type Mismatch in assignment!\n")
	end

fun printFormal (Formal(typ, id)) = 
	(
		printType typ;
		print " ";
		print id;
		(variableList := (id, typ, (!level)) :: (!variableList))
	)

fun mapFormal id (Formal(typ, _)) = 
	let
		val temp = if AtomMap.inDomain(!functionMap, Atom.atom id) then AtomMap.lookup(!functionMap, Atom.atom id) else []
	in
		(functionMap := AtomMap.insert(!functionMap, Atom.atom id, typ :: temp))
	end

fun printFormals id [] 		= (functionMap := AtomMap.insert(!functionMap, Atom.atom id, []))
|	printFormals id [x] 	= ((printFormal x); mapFormal id x)
|	printFormals id (x::xs)	= (printFormal x; mapFormal id x; print ", "; printFormals id xs)

fun printMethodDec (MethodDec(typ, "main", flist, vlist, slist)) = 
	(
		printTabs();
		print "public static void main(";
		(functionList := ("main", voidType, (!level)) :: (!functionList));
		levelUp ();
		printFormals "main" flist;
		print ")\n";
		printTabs();
		print "{\n";
		more();
		map printVarDec vlist;
		map printStatement slist;
		less();
		printTabs();
		print "}\n";
		levelDown ()
	)
|	printMethodDec (MethodDec(typ, id, flist, vlist, slist)) = 
	(
		printTabs();
		print "public ";
		printType typ;
		print " ";
		print id;
		(functionList := (id, typ, (!level)) :: (!functionList));
		print "(";
		levelUp();
		printFormals id flist;
		print ")\n";
		printTabs();
		print "{\n";
		more();
		map printVarDec vlist;
		map printStatement slist;
		less();
		printTabs();
		print "}\n";
		levelDown()

	)	

fun printClassDec (ClassDec (id, varDecList, methodDecList)) = 
	(
		printTabs();
		print "class ";
		print id;
		print "\n";
		printTabs();
		print "{\n";
		more();
		levelUp();
		map printVarDec varDecList;
		map printMethodDec methodDecList;
		less();
		printTabs();
		print "}\n";
		levelDown()
	)

fun printProgram (Program x) = map printClassDec x

(* fun compileProgram x = printProgram x *)

end