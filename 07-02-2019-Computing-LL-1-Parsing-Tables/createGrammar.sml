structure myGrammar = Grammar
val p = print("Enter the list of tokens (separated by <space>): ")
val tString = Option.valOf(TextIO.inputLine TextIO.stdIn)
fun f #" " 	= true
|	f #"\n" = true
|	f _		= false
val tList = String.tokens f tString
val t = map myGrammar.addToken tList
val p = print("Enter the list of symbols (separated by <space>): ")
val sString = Option.valOf(TextIO.inputLine TextIO.stdIn)
val sList = String.tokens f sString
val t = map myGrammar.addSymbol sList

fun g #">" 	= true
|	g _		= false

fun h #"|"	= true
|	h _		= false

val p = print("Enter the list of rules: \n")

(* val r1 = Option.valOf(TextIO.inputLine TextIO.stdIn)
val r2 = String.tokens g r1
val s = List.hd (String.tokens f (List.hd r2))
val r3 = List.hd(List.tl r2)
val r4 = String.tokens h r3
val r5 = map (String.tokens f) r4
val r = myGrammar.addRule s r5 *)

val r1 = ref ""
val r2 : string list ref = ref []
val s = ref ""
val r3 = ref ""
val r4 : string list ref = ref []
val r5 : string list list ref = ref []
fun fr1 () = (r1 := Option.valOf(TextIO.inputLine TextIO.stdIn))
fun fr2 () = (r2 := String.tokens g (!r1))
fun fs	() = (s := List.hd (String.tokens f (List.hd (!r2))))
fun fr3 () = (r3 := List.hd(List.tl (!r2)))
fun fr4	() = (r4 := String.tokens h (!r3))
fun fr5 () = (r5 := map (String.tokens f) (!r4))
fun fr	() = (myGrammar.addRule (!s) (!r5))
fun frs	() = (
				fr1(); 
				if
					((!r1) = "\n")
				then
					()
				else
					(fr2(); fs(); fr3(); fr4(); fr5(); fr(); frs())
			)

val t = frs()
(* val t = myGrammar.printRules() *)

(* val t = myGrammar.addToken "$"
val t = myGrammar.addToken "+"
val t = myGrammar.addToken "*"
val t = myGrammar.addToken "N"
val t = myGrammar.addSymbol "S"
val t = myGrammar.addSymbol "E"
val t = myGrammar.addSymbol "E'"
val t = myGrammar.addSymbol "T"
val t = myGrammar.addSymbol "T'" *)
(* val t = myGrammar.addRule "S" [["E", "$"]]
val t = myGrammar.addRule "E" [["T", "E'"]]
val t = myGrammar.addRule "E'" [["+", "T", "E'"], []]
val t = myGrammar.addRule "T" [["N", "T'"]]
val t = myGrammar.addRule "T'" [["*", "N", "T'"], []] *)

(* val t = myGrammar.addToken "a"
val t = myGrammar.addToken "b"
val t = myGrammar.addSymbol "C"
val t = myGrammar.addSymbol "A"
val t = myGrammar.addSymbol "B"
val t = myGrammar.addSymbol "D"
val t = myGrammar.addRule "C" [["D", "A"]]
val t = myGrammar.addRule "A" [["a", "A"]]
val t = myGrammar.addRule "B" [["b", "B"], []]
val t = myGrammar.addRule "D" [["B", "B"]] *)

(* val x = myGrammar.addSymbol "A";
val x = myGrammar.addSymbol "B";
val x = myGrammar.addSymbol "C";
val x = myGrammar.addSymbol "D";
val x = myGrammar.addToken "a";
val x = myGrammar.addRule "A" [["B"]];
val x = myGrammar.addRule "B" [["C"]];
val x = myGrammar.addRule "C" [["D"]];
val x = myGrammar.addRule "D" [["a"]]; *)