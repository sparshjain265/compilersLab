structure Printing = struct

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

fun printBasic Bool = print "bool"
|	printBasic Int	= print "int"

fun printType (basicType b) = printBasic b
|	printType (arrayType b) = (printBasic b; print "[]")
|	printType (objType id)	= print id
|	printType voidType		= print "void"

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

fun printConstant (Cint x) = print(Int.toString x)
|	printConstant (Cbool x) = print(Bool.toString x)

fun printExp (Const c) = printConstant c
|	printExp (BINOP(bop, e1, e2)) = (printExp e1; print " "; printBinop bop; print " "; printExp e2)
|	printExp (RELOP(rop, e1, e2)) = (printExp e1; print " "; printRelop rop; print " "; printExp e2)
|	printExp (NOT(e)) = (print"!"; printExp e)
|	printExp (ArrayElement(e1, e2)) = (printExp e1; print "["; printExp e2; print "]")
|	printExp (ArrayLength(e)) = (printExp e; print".length")
|	printExp (Call(e, id, eList)) = (printExp e; print "."; print id; print "("; printExps eList; print ")")
|	printExp (NewArray(b, e)) = (print "new "; printBasic b; print "["; printExp e; print "]")
|	printExp (NewObject(id)) = (print "new "; print id; print"()")
|	printExp (Member(e, id)) = (printExp e; print "."; print id)
|	printExp This	= print "this"
|	printExp (Var(id)) = print id
and printExps [] 		= ()
|	printExps [e] 		= printExp e
|	printExps (x :: xs)	= (printExp x; print ", "; printExps xs)

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
	(
		printTabs();
		if(isSome(e1)) then (printExp(valOf e1); print ".") else ();
		print id;
		if(isSome(e2)) then (print "["; printExp(valOf e2); print "]") else ();
		print " = ";
		printExp e;
		print ";\n"
	)
|	printStatement (CallStmt(e, id, elist)) = 
	(
		printTabs();
		printExp e;
		print ".";
		print id;
		print "(";
		printExps elist;
		print ");\n"
	)
|	printStatement (If(e, s1, Block [])) = 
	(
		printTabs();
		print "if(";
		printExp e;
		print ") then\n";
		more();
		printStatement s1;
		less()
	)
|	printStatement (If(e, s1, s2)) = 
	(
		printTabs();
		print "if(";
		printExp e;
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
|	printStatement (While(e, s)) = 
	(
		printTabs();
		print "while(";
		printExp e;
		print ")\n";
		more();
		printStatement s;
		less()
	)
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
		if(isSome(e)) then (print " "; printExp(valOf e)) else ();
		print ";\n"
	)

fun printVarDec (VarDec(typ, id, exp)) = 
	(
		printTabs();
		printType typ;
		print " ";
		print id;
		if(isSome(exp)) then (print " = "; printExp(valOf exp)) else ();
		print ";\n"
	)

fun printFormal (Formal(typ, id)) = 
	(
		printType typ;
		print " ";
		print id
	)

fun printFormals [] 		= ()
|	printFormals [x] 		= printFormal x
|	printFormals (x::xs)	= (printFormal x; print ", "; printFormals xs)

fun printMethodDec (MethodDec(typ, "main", flist, vlist, slist)) = 
	(
		printTabs();
		print "public static void main(";
		printFormals flist;
		print ")\n";
		printTabs();
		print "{\n";
		more();
		map printVarDec vlist;
		map printStatement slist;
		less();
		printTabs();
		print "}\n"
	)
|	printMethodDec (MethodDec(typ, id, flist, vlist, slist)) = 
	(
		printTabs();
		print "public ";
		printType typ;
		print " ";
		print id;
		print "(";
		map printFormal flist;
		print ")\n";
		printTabs();
		print "{\n";
		more();
		map printVarDec vlist;
		map printStatement slist;
		less();
		printTabs();
		print "}\n"
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
		map printVarDec varDecList;
		map printMethodDec methodDecList;
		less();
		printTabs();
		print "}\n"
	)

fun printProgram (Program x) = map printClassDec x

fun compileProgram x = printProgram x

end