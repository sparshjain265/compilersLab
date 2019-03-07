structure lr1Item = struct
	open flGrammar

	(* find follow *)
	val fl = findFollow();

	type Item = {
					lhs		:	Atom.atom,			(*Left Hand side of the rule (symbol)*)
					before	:	Atom.atom list,		(*Right Hand side of the rule before the '.' in reverse order*)
					after	:	Atom.atom list,		(*Right Hand side of the rule after the '.' *)
					wait	:	AtomSet.set
				}

	structure ItemKey : ORD_KEY = struct
		type ord_key = Item

		fun 	compare_atom_list ([], [])			= EQUAL
			|	compare_atom_list ([], x)			= LESS
			|	compare_atom_list (x, [])			= GREATER
			|	compare_atom_list (x::xs, y::ys)	= case Atom.compare (x, y) of
															LESS		=> LESS
			 											|	GREATER		=> GREATER
														|	EQUAL		=> compare_atom_list (xs, ys)

		fun compare (x: ord_key, y: ord_key)	=	let
														val lhs1 = #lhs x
														val lhs2 = #lhs y
														val before1 = #before x
														val before2 = #before y
														val after1 = #after x
														val after2 = #after y
														val wait1 = #wait x
														val wait2 = #wait y
													in
														case (Atom.compare (lhs1, lhs2), compare_atom_list(before1, before2), compare_atom_list(after1, after2), AtomSet.compare(wait1, wait2)) of
																(LESS, _, _, _)					=> LESS
															|	(GREATER, _, _, _) 				=> GREATER
															|	(EQUAL, LESS, _, _)				=> LESS
															|	(EQUAL, GREATER, _, _)			=> GREATER
															|	(EQUAL, EQUAL, LESS, _)			=> LESS
															|	(EQUAL, EQUAL, GREATER, _)		=> GREATER
															|	(EQUAL, EQUAL, EQUAL, LESS)		=> LESS
															|	(EQUAL, EQUAL, EQUAL, GREATER)	=> GREATER
															|	(EQUAL, EQUAL, EQUAL, EQUAL)	=> EQUAL 
													end
	end

	structure ItemSet = RedBlackSetFn(ItemKey)

	fun printAtom x = print(Atom.toString x)

	fun printItem (x: Item) =	let
									val a = #lhs x
									val b = #before x
									val c = #after x
									val d = #wait x
								in
									(
										printAtom a;
										print(" -> ");
										map printAtom (List.rev b);
										print(".");
										map printAtom c;
										print(" , ");
										printAtomSet d
										(* print("\n") *)
									)
								end

	fun printItemSet (x: ItemSet.set) = 
		let
			val xList = ItemSet.listItems x
		in
			map printItem xList
		end


(* val aItem = { lhs    = Atom.atom "A",
              before = List.map Atom.atom ["A", "a"],
              after  = List.map Atom.atom ["b", "B"],
			  wait = AtomSet.singleton(Atom.atom "$")
            }

val t = printItem aItem *)

	fun closureOne (x: ItemSet.set) =	
		let
			val t = ref x
			val xList = ItemSet.listItems x
			fun f (x: Item) =	
				let
					val a = #lhs x
					val b = #before x
					val c = #after x
					val d = #wait x
				in
					case List.null c of
							true	=>	[()]
						|	false	=>	
										let
											val h = List.hd c
										in
											if
												AtomSet.member((!symbols), h)
											then
												let
													val p = AtomMap.lookup((!rules), h)
													val pList = RHSSet.listItems p
													val waitList = AtomSet.listItems d
													val tail = List.tl c
													
													fun getNewWaitListOne z = firstOfAtomList(tail @ [z])
													val newWaitList = 
																		if
																			List.null waitList
																		then
																			[firstOfAtomList tail]
																		else
																			map getNewWaitListOne waitList
													
													fun getNewWait [] = AtomSet.empty
													|	getNewWait (x :: xs) = AtomSet.union(x, getNewWait xs)

													val newWait = getNewWait newWaitList

													fun g (x: Atom.atom list) = 
														let
															val tempItem = {
																				lhs = h,
																				before = [],
																				after = x,
																				wait = newWait
																			}
														in
															(t := ItemSet.add((!t), tempItem))
														end
												in
													map g pList
												end
											else
												[()]
										end
				end
		in
			(map f xList; (!t))
		end

	fun closureFull (x: ItemSet.set) =	
		let
			val t = closureOne x
			val c = ItemSet.compare(x, t)
		in
			case c of
				EQUAL 	=> t
			|	_		=> closureFull t
		end

	(* val t = closureFull(ItemSet.singleton({lhs = Atom.atom "S", before = [], after = List.map Atom.atom ["E", "$"], wait = AtomSet.singleton(Atom.atom "$")}))
	val t1 = ItemSet.add(t, {lhs = Atom.atom "E'", before = List.map Atom.atom ["+", "T"], after = List.map Atom.atom ["E'"], wait = AtomSet.singleton(Atom.atom "+")})
	val t2 = closureFull(t1)
	val p = printItemSet t2 *)


end