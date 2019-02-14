structure ll1 = struct
	open flGrammar;

	(* Find Follow *)
	val fl = findFollow();

	(* Function takes a rule A - > x and returns a set of tokens t such that the rule belongs in (A, t) entry of LL1 Parsing Table*)
	fun locateOneRule (A, x)	=	let
										val t1 = firstOfAtomList x
										val t = if
													checkRuleNullable x
												then
													AtomSet.union(t1, AtomMap.lookup((!followOfAll), A))										
												else
													t1
									in
										t
									end
	
	val LL1 : ((Atom.atom * AtomSet.set * Atom.atom list) list) ref = ref []

	fun addOneRule (A, x)	=	let
									val s = locateOneRule (A, x)
								in
									LL1 := ((A, s, x) :: (!LL1))
								end

	fun addOneProduction (A, x)	=	let
										val l = RHSSet.listItems x
										fun f x = addOneRule(A, x)
									in
										map f l
									end
	
	fun findLL1 () = map addOneProduction (AtomMap.listItemsi(!rules))

	fun printLL1 []					=	()
	|	printLL1 ((a, b, c) :: xs)	=	let
											val l = AtomSet.listItems b
											fun f x	=	(
															print("(");
															printAtom a;
															print(", ");
															printAtom x;
															print(")\t:\t");
															printAtom a;
															print("\t->\t");
															map printAtom c;
															print("\n")
														)
										in
											(map f l; printLL1 xs)
										end
	val t1 = findLL1()
	val t2 = printLL1(!LL1)
end