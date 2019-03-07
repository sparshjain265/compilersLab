structure lr1states =
struct
	open lr1Item
	
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

end