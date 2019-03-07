	structure lr0 = 
struct
	open lr0states

	(* number of states *)
	val nStates = ref 0
	(* maps from state to atom and vice versa for the states included so far *)
	val StateToAtomMap : stateToAtom ref = ref stateMap.empty
	val AtomToStateMap : atomToState ref = ref AtomMap.empty

	(* edges are from one state to other states represented as atoms on a symbol or token represented as atoms *)
	(* first atom in value is symbol or token, second is state *)
	type eType = Atom.atom * Atom.atom
	type edgeType = (eType list) AtomMap.map
	val emptyEdge : eType list = []
	val edges : edgeType ref = ref AtomMap.empty

	(* takes a state x and if already present, give the corresponding atom
		else insert and give corresponding atom *)
	fun getState x = if
						stateMap.inDomain((!StateToAtomMap), x)
					then
						stateMap.lookup((!StateToAtomMap), x)
					else
						let
							val t1 = nStates := 1 + !nStates
							val t2 = Atom.atom (Int.toString (!nStates))
						in
							(
								StateToAtomMap := stateMap.insert((!StateToAtomMap), x, t2);
								AtomToStateMap := AtomMap.insert((!AtomToStateMap), t2, x);
								edges := AtomMap.insert((!edges), t2, emptyEdge);
								t2
							)
						end

	(* gives next state from I on shift x (token) or goto x (symbol)*)
	fun nextState (I : ItemSet.set, x : Atom.atom) = 
		let
			val newSet = ref ItemSet.empty
			fun f (y : Item) = 
				let
					val l = #lhs y
					val b = #before y
					val a = #after y
				in
					if
						List.null(a)
					then
						()
					else
						let
							val z = List.hd a
						in
							if
								Atom.same(z, x)
							then
								let
									val item = {
										lhs = l,
										before = z :: b,
										after = List.tl a
									}
								in
									newSet := ItemSet.union((!newSet), ItemSet.singleton item)
								end
							else
								()
						end
				end
		in
			(
				map f (ItemSet.listItems I);
				closureFull(!newSet)
			)
		end

	(* start state in grammar should be "'S" and it should have only 1 production *)
	val start = List.hd(RHSSet.listItems(AtomMap.lookup((!rules), Atom.atom "S'")))
	val startItem = {
						lhs = Atom.atom "S'",
						before = List.map Atom.atom [],
						after = start
					}

	val startState = closureFull(ItemSet.singleton startItem)
	val insertStartState = getState startState

	fun printAtomToState stateAtom =
		let
			fun f (key, value) = (
									print "State: ";
									printAtom key;
									print "\n";
									printItemSet value
								)
			
			fun printReduce value = 
				let
					val itemList = ItemSet.listItems value
					fun printReduceOnce i = 
						let
							val l = #lhs i
							val b = #before i
							val a = #after i
							val tempFollow = AtomMap.lookup((!followOfAll), l)
						in
							if
								List.null a andalso not (AtomSet.isEmpty tempFollow)
							then
								(
									print "Reduce by ";
									printAtom l;
									print " -> ";
									map printAtom (List.rev b);
									print " on input ";
									printAtomSet tempFollow
								)
							else
								()
						end
				in
					map printReduceOnce itemList
				end	
				

			fun g (key, value) = 
				let
					val nexts = AtomMap.lookup((!edges), key)
					fun h (x, y) = 
						if
							AtomSet.member((!tokens), x)
						then
							(
								print "Shift on ";
								printAtom x;
								print " to State ";
								printAtom y;
								print "\n"
							)
						else
							(
								print "Goto on ";
								printAtom x;
								print " to State ";
								printAtom y;
								print "\n"
							)
				in
					(
						f(key, value);
						printReduce value;
						map h nexts;
						print "\n"
					)
				end
		in
			map g (AtomMap.listItemsi stateAtom)
		end
	
	fun printStateToAtom x = 
		let
			fun f (key, value) = (
									printItemSet key;
									print "State: ";
									printAtom value;
									print "\n\n"
								)
		in
			map f (stateMap.listItemsi x)
		end

	(* takes a state and generates states for shift and goto actions *)
	fun genStateOne I = 
		let
			val ins = (AtomSet.listItems(!symbols))@(AtomSet.listItems(!tokens))
			fun f input = 
				let
					val newState = nextState (I, input)
				
				in
					if
						ItemSet.isEmpty newState
					then
						()
					else
						let
							val prevEdge = AtomMap.lookup((!edges), getState I)
							val e = (input, getState newState)
							fun g ((x,y):eType) = Atom.same(x, input) andalso Atom.same(y, getState newState)
							val newEdge : eType list = if
															List.exists g prevEdge
														then
															prevEdge
														else 
															e :: prevEdge

						in
							edges := AtomMap.insert((!edges), getState I, newEdge)
						end
				end
		in
			map f ins
		end
	
	fun genState () = 
		let
			val states = AtomMap.listItems(!AtomToStateMap)
		in
			map genStateOne states
		end

	fun genAllStates () = 
		let
			val temp = (!nStates)
			val t = genState ()
		in
			if
				temp < (!nStates)
			then
				genAllStates ()
			else
				()
		end
	

	val t = genAllStates()
	val p = printAtomToState(!AtomToStateMap)

end