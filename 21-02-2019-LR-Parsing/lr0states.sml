structure lr0states =
struct
	open lr0Item
	
	structure stateKey : ORD_KEY = 
	struct 
		(* open ItemSet *)
		type ord_key = ItemSet.set
		val compare = ItemSet.compare
	end

	(* map from atom to state *)
	type atomToState = ItemSet.set AtomMap.map

	(* map from state to atom *)
	structure stateMap = RedBlackMapFn(stateKey)
	type stateToAtom = Atom.atom stateMap.map

	fun printAtomToState x =
		let
			fun f (key, value) = (
									print "State: ";
									printAtom key;
									print "\n";
									printItemSet value;
									print "\n"
								)
		in
			map f (AtomMap.listItemsi x)
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

end