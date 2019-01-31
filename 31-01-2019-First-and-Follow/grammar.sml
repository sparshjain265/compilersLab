type RHS = Atom.atom list

structure RHS_KEY : ORD_KEY =
struct
	type ord_key = RHS

	(* fun compare (x, y) = if List.length x > List.length y
								then GREATER
								else if List.length x < List.length y
									then LESS 
									else EQUAL *)

	fun 	compare ([], [])			= EQUAL
		|	compare ([], x)			= LESS
		|	compare (x, [])			= GREATER
		|	compare (x::xs, y::ys)	= case Atom.compare (x, y) of
												LESS		=> LESS
		 									|	GREATER	=> GREATER
											|	EQUAL		=> compare (xs, ys)
	
end

structure RHSSet = RedBlackSetFn(RHS_KEY)

type Productions = RHSSet.set

type Rules = Productions AtomMap.map

type Grammar    = { symbols : AtomSet.set, tokens : AtomSet.set, rules : Rules }

val symbols = ref AtomSet.empty
val tokens	= ref AtomSet.empty

fun addSymbol	x	=	let 
								val sym = Atom.atom x
							in
								symbols := AtomSet.add(!symbols, sym)
							end

fun addToken	x	=	let
								val tok = Atom.atom x
							in 
								tokens := AtomSet.add(!tokens, tok)
							end

fun	printAtomList [] 			= (print "\n")
	|	printAtomList (x::xs)	= (print ((Atom.toString x) ^ " "); printAtomList xs)

fun printAtomListList x = map printAtomList x

fun printAtomSet x =	let 
								val xList = AtomSet.listItems x
							in
								printAtomList xList
							end

fun printSymbols () = printAtomSet (!symbols)
fun printTokens () = printAtomSet (!tokens)

fun toAtomList x = map Atom.atom x
fun toAtomListList x = map toAtomList x

fun toProduction x = RHSSet.fromList (toAtomListList x)

val rules:Rules ref = ref AtomMap.empty

fun addRule A x = (rules := AtomMap.insert(!rules, (Atom.atom A), (toProduction x)))

fun printRule (A, x) = (print(Atom.toString A); print(" " ^ "->" ^ " "); printAtomListList(RHSSet.listItems x))

fun printRules () = map printRule (AtomMap.listItemsi (!rules))