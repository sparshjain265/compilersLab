structure nGrammar = struct

	open myGrammar

	(* val t = checkSetMember (!symbols) (Atom.atom "E")  *)

	(* Checks whether x is a member of S *)
	fun checkSetMember S x = AtomSet.member(S, x)

	(* nullable is a set of nullable symbols*)
	val nullable = ref AtomSet.empty

	(* Takes a list of atoms/symbols-tokens and returns true if all are nullable *)
	(* Takes a production and returns true if it is nullable *)
	fun	checkRuleNullable [] 		= true
	|	checkRuleNullable (x::xs)	= checkSetMember (!nullable) x andalso checkRuleNullable xs

	(* Takes a list of productions of a symbol and returns true if any production is nullable *)
	fun checkNullable [] 			= false
	|	checkNullable (x::xs)		= checkRuleNullable x orelse checkNullable xs

	(* takes list of productions of A as x, if any of the productions is nullable, that is if A is nullable, add A to nullable *)
	fun passRule (A, x) =	let
								val l = RHSSet.listItems x	
							in
								if 
									(checkNullable l) = true
								then
									(nullable := AtomSet.add(!nullable, A))
								else
									()
							end

	fun passAllRules () = map passRule (AtomMap.listItemsi(!rules))

	fun findNullable () =	let
								val x = (!nullable)
								val y = passAllRules ()
							in
								if
									AtomSet.equal (x, (!nullable))
								then
									()
								else
									findNullable ()
							end
	
	fun printNullable () = printAtomSet (!nullable)

end