structure fGrammar = struct
	open nGrammar

	(* find nullables *)
	val n = findNullable ()

	(* first of all is a map from atom (symbol or token) to set of atoms (set of tokens) *)
	val firstOfAll : AtomSet.set AtomMap.map ref = ref AtomMap.empty

	(* Populate the map with tokens for first of tokens *)
	fun firstOfToken x = (firstOfAll := AtomMap.insert ( (!firstOfAll), x, AtomSet.fromList([x]) ) )
	fun firstOfAllTokens () = map firstOfToken (AtomSet.listItems (!tokens))
	(* val t = firstOfAllTokens (!tokens) *)

	(* Populate the map with empty for first of symbols *)
	fun initialSymbol x = (firstOfAll := AtomMap.insert ( (!firstOfAll), x, AtomSet.empty ) )
	fun initialAllSymbols () = map initialSymbol (AtomSet.listItems (!symbols))
	(* val t = initialAllSymbols (!symbols) *)

	(* Function to print First *)
	fun printFirst () 	=	let
								val x = AtomMap.listItemsi (!firstOfAll)
								fun f (a, b) = (printAtom a; print(": "); printAtomSet b)
							in
								map f x
							end

	(* 
		Function to calculate first of a list of atoms
		Used to calculate first of symbols through its rules
		Can also be used to populate LL(1) table later
	*)
	fun firstOfAtomList [] 		=	AtomSet.empty
	|	firstOfAtomList (x::xs)	=	if (* if x is nullable, find first of x and xs, and return the union*)
										AtomSet.member((!nullable), x)
									then
										let
											val f1 = AtomMap.lookup((!firstOfAll), x)
											val f2 = firstOfAtomList xs
										in
											AtomSet.union(f1, f2)
										end
									else (* else return first of only x*)
										AtomMap.lookup((!firstOfAll), x)
	

	(* Function to calculate first of A from x, A is a symbol, A -> x are rules, so x is set of atom list *)
	fun firstOfRule (A,x)	=	let
								(* l is now list of atom list (list of productions from A to p) *)
								val l = RHSSet.listItems x
								(* initialise temp to empty set *)
								val temp = ref AtomSet.empty
								fun addInTemp x = (temp := AtomSet.union((!temp), firstOfAtomList x))
							in
								(* union through first calculated for all rules in l*)
								(* then update in firstOfAll *)
								(map addInTemp l; firstOfAll := AtomMap.insert((!firstOfAll), A, AtomSet.union(AtomMap.lookup((!firstOfAll), A), (!temp))))
							end

	(* Goes through the whole grammar once to update the first *)
	fun firstOfGrammar () = map firstOfRule (AtomMap.listItemsi(!rules))

	fun findFirstIter 1 = firstOfGrammar ()
	|	findFirstIter n = (firstOfGrammar; findFirstIter (n-1))

	fun findFirst () = (firstOfAllTokens(); initialAllSymbols(); findFirstIter (AtomSet.numItems(!symbols)) )

end