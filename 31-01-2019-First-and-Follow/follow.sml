structure flGrammar = struct
	open fGrammar

	(* find nullables *)
	val f = findFirst ()

	(* follow of all is a map from atom (symbol or token) to set of atoms (set of tokens) *)
	val followOfAll : AtomSet.set AtomMap.map ref = ref AtomMap.empty

	(* Populate the map with empty for first of symbols *)
	fun initialSymbolFollow x = (followOfAll := AtomMap.insert ( (!followOfAll), x, AtomSet.empty ) )
	fun initialAllSymbolsFollow () = map initialSymbolFollow (AtomSet.listItems (!symbols))

	(* Function to print First *)
	fun printFollow () 	=	let
								val x = AtomMap.listItemsi (!followOfAll)
								fun f (a, b) = (printAtom a; print(": "); printAtomSet b)
							in
								map f x
							end

	(* Used to calculate follow of symbols through a rule A -> r *)
	fun followFromAtomList (A, []) 		= 	()
	|	followFromAtomList (A, (x::xs))	=	let
												val fA = AtomMap.lookup((!followOfAll), A)
											in
												if	(* if x is a symbol, find follow of x *)
													AtomSet.member((!symbols), x)
												then
													let
														val f1 = AtomMap.lookup((!followOfAll), x)
														val f2 = firstOfAtomList xs
														val f3 = AtomSet.union(f1, f2)
														val f4 = AtomSet.union(f3, fA)
													in
														if (* if xs is nullable, then follow of x = (follow of x till now) union first of xs union follow of A *)
															checkRuleNullable xs
														then
															(followOfAll := AtomMap.insert((!followOfAll), x, f4); followFromAtomList (A, xs))
														else (* else follow of x = (follow of x till now) union first of xs *)
															(followOfAll := AtomMap.insert((!followOfAll), x, f3); followFromAtomList (A, xs))
													end
												else
													followFromAtomList (A, xs)
											end
	
	(* Function to calculate follow from rules A to x where x is set of productions from A *)
	fun followFromRule (A, x)	=	let
										val l = RHSSet.listItems x
										fun f x = followFromAtomList (A, x)
									in
										map f l
									end
	
	(* Goes through the whole grammar once to update the follow *)
	fun followOfGrammar () = map followFromRule (AtomMap.listItemsi(!rules))

	fun findFollowIter 1 = followOfGrammar ()
	|	findFollowIter n = (firstOfGrammar (); findFollowIter (n-1))

	fun findFollow () = (initialAllSymbolsFollow(); findFollowIter (AtomSet.numItems(!symbols) * AtomSet.numItems(!tokens)))

end