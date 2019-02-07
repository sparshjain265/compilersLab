(* Structure Grammar to capture the rules of a grammar *)
(* symbols  and tokens are atoms, right hand side of a rule is thus a list of atom *)
structure Grammar = struct

	(* RHS is a list of atom *)
	type RHS = Atom.atom list

	(* define ordering on RHS to create AtomSet and AtomMap *)
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

	(* RHSSet is an AtomSet of atom list *)
	structure RHSSet = RedBlackSetFn(RHS_KEY)

	(* Productions is thus a set of atom list *)
	type Productions = RHSSet.set

	(* Rules are a map from atom to productions *)
	type Rules = Productions AtomMap.map

	(* 
		symbols	: set of atoms
		tokens	: set of atoms
		rules		: map from atom to set of atom list (symbol to production)
	*)
	type grammar    = { symbols : AtomSet.set, tokens : AtomSet.set, rules : Rules }

	(* reference variables to store symbols and tokens of the grammar *)
	val symbols = ref AtomSet.empty
	val tokens	= ref AtomSet.empty

	(* function to take a string and add it as a symbol in the grammar *)
	fun addSymbol	x	=	let 
									val sym = Atom.atom x
								in
									symbols := AtomSet.add(!symbols, sym)
								end

	(* function to take a string and add it as a token in the grammar *)
	fun addToken	x	=	let
									val tok = Atom.atom x
								in 
									tokens := AtomSet.add(!tokens, tok)
								end

	(* takes an atom and prints it *)
	fun printAtom x	=	print((Atom.toString x) ^ " ")
	
	(* prints a list of atoms with a newline *)
	fun printAtomList x = (map printAtom x; print "\n")

	(* prints a set of atoms *)
	fun printAtomSet x =	let 
									val xList = AtomSet.listItems x
								in
									printAtomList xList
								end

	(* functions to print symbols and tokens of the grammar *)
	fun printSymbols () = printAtomSet (!symbols)
	fun printTokens () = printAtomSet (!tokens)

	(* converts a list of string to a list of atoms *)
	fun toAtomList x = map Atom.atom x
	(* converts a list of list of string to a list of list of atoms *)
	fun toAtomListList x = map toAtomList x

	(* takes a list of list of string and creates a set of list of atoms that is productions *)
	fun toProduction x = RHSSet.fromList (toAtomListList x)

	(* variable to store rules of the grammar *)
	val rules:Rules ref = ref AtomMap.empty

	(* function to take a rule as string to list of list of string TO a symbol to production *)
	fun addRule A x = (rules := AtomMap.insert(!rules, (Atom.atom A), (toProduction x)))

	(* function to print all the rules of a symbol*)
	fun printRule (A, x) = 	let
										val l = RHSSet.listItems x
										fun f x = (printAtom A; print(" " ^ "->" ^ " "); printAtomList x)
									in
										map f l
									end

	(* function to print all the rules of the grammar *)
	fun printRules () = map printRule (AtomMap.listItemsi (!rules))

end