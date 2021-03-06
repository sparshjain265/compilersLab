(*  ************** Driver file  **************** *)
structure Driver = struct 

(*  *******   Tie all the libraries together ******** *)

structure subJavaLrVals =
	subJavaLrValsFun(structure Token = LrParser.Token);
structure subJavaLex =
	subJavaLexFun(structure Tokens = subJavaLrVals.Tokens);
structure subJavaParser =
	Join(structure ParserData = subJavaLrVals.ParserData
			 structure Lex = subJavaLex
			 structure LrParser = LrParser);

(* ******** Build a lexer and Parser *************** *)

fun makesubJavaLexer strm = subJavaParser.makeLexer (fn n => TextIO.inputN(strm,n))
val makeFileLexer      = makesubJavaLexer o TextIO.openIn

val oFile = ref "a.js"

val thisLexer = case CommandLine.arguments() of
				[]  => makesubJavaLexer TextIO.stdIn
		 |  [x] => makeFileLexer x
		 |  [x, y] => (oFile := y; makeFileLexer x)
		 |  _   => (TextIO.output(TextIO.stdErr, "usage: driver file"); OS.Process.exit OS.Process.failure)

fun print_error (s,i:int, j:int) = TextIO.output(TextIO.stdErr,
							"Error:\tline " ^ (Int.toString i) ^ "." ^ (Int.toString j) ^ ":\t" ^ s ^ "\n")

val (program,_) = subJavaParser.parse (0,thisLexer,print_error,())
val check  = Semantic.checkProgram program

structure os : OSTREAMIN = 
struct
	val os = TextIO.openOut (!oFile)
end

structure codeGenerator = codeGen (os)

val gen = if check then 
						(print "Error! Compilation aborted\n") 
					else 
						(
							codeGenerator.generate program; 
							print ("Successfully compiled to " ^ (!oFile) ^ "\n")
						)

val oclose = TextIO.closeOut os.os

end (* struct Driver *)