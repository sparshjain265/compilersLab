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
	(* fun followFromAtomList (A, []) 		= 	()
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
											end *)

	fun followFromAtomList (A , []) = ()
    | followFromAtomList (A ,  (x :: xs)) = (
                                        if AtomSet.member ( (!symbols , x) ) = true 
                                        then 
                                            (if checkRuleNullable xs = true
                                            then 
                                                let
                                                val t0 = AtomMap.lookup ( (!followOfAll) , x) 
                                                val t1 = firstOfAtomList xs
                                                val t2 = AtomMap.lookup ( (!followOfAll) , A )
                                                val t3 = AtomSet.union (t1 , t2)
                                                in
                                                    (followOfAll := AtomMap.insert ( (!followOfAll) , x , AtomSet.union (t0 , t3) )   )
                                                end
                                            else
                                                let 
                                                val t0 = AtomMap.lookup ( (!followOfAll) , x)
                                                val t1 = firstOfAtomList xs
                                                in 
                                                    (followOfAll := AtomMap.insert ( (!followOfAll) , x , AtomSet.union (t0 , t1)  ))
                                                end
                                         ; followFromAtomList (A , xs) )
                                    else
                                    followFromAtomList (A , xs)
                                   )
	
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
	|	findFollowIter n = (followOfGrammar (); findFollowIter (n-1))

	fun findFollow () = (initialAllSymbolsFollow(); findFollowIter (AtomSet.numItems(!symbols) * AtomSet.numItems(!tokens)))

end

(* structure flGrammar = struct 

open fGrammar;

val f = findFirst()
val x = print("=========== This is for Follow ========== \n")

(* Creating the map that is going to contain the follow of each of the symbols *)
type set = AtomSet.set 
type atomMap = set AtomMap.map
val followAllSymbols: atomMap ref = ref AtomMap.empty

(* Inserting the symbols in the followAllSymbols with the empty atom set *)
fun putSymbolFollow x = (followAllSymbols := AtomMap.insert ((!followAllSymbols) , x , AtomSet.empty ))
fun insertSymbolsFollow x = map putSymbolFollow (AtomSet.listItems x)
val t = insertSymbolsFollow (!symbols)

(* This function takes the list of atoms and for all the symbols in the list find its follow
with current info and put it in the map *)
fun followAtomList (A , []) = ()
    | followAtomList (A ,  (x :: xs)) = (
                                        if AtomSet.member ( (!symbols , x) ) = true 
                                        then 
                                            (if checkRuleNullable xs = true
                                            then 
                                                let
                                                val t0 = AtomMap.lookup ( (!followAllSymbols) , x) 
                                                val t1 = firstOfAtomList xs
                                                val t2 = AtomMap.lookup ( (!followAllSymbols) , A )
                                                val t3 = AtomSet.union (t1 , t2)
                                                in
                                                    (followAllSymbols := AtomMap.insert ( (!followAllSymbols) , x , AtomSet.union (t0 , t3) )   )
                                                end
                                            else
                                                let 
                                                val t0 = AtomMap.lookup ( (!followAllSymbols) , x)
                                                val t1 = firstOfAtomList xs
                                                in 
                                                    (followAllSymbols := AtomMap.insert ( (!followAllSymbols) , x , AtomSet.union (t0 , t1)  ))
                                                end
                                         ; followAtomList (A , xs) )
                                    else
                                    followAtomList (A , xs)
                                   )


(* This function goes through production of only one symbol and update the follow *)
(* So A is atom and x is set of list of atoms *)
fun followOneProduction (A, x) = let
                                    val t = RHSSet.listItems x
                                    fun f x = followAtomList (A , x)
                              in 
                                    map f t
                              end

(* This function goes through the whole grammar ones *)
fun followOneIter () = map followOneProduction (AtomMap.listItemsi (!rules))

(*Running for n iterations*)
fun findFollow n = if n > 1 then (followOneIter () ; findFollow (n-1) ) else followOneIter()

(*finding the follow*)
val t = findFollow (AtomSet.numItems (!symbols) )
fun printFollow () 	=	let
							val x = AtomMap.listItemsi (!followAllSymbols)
							fun f (a, b) = (printAtom a; print(": "); printAtomSet b)
						in
							map f x
						end
val x = printFollow()



end *)