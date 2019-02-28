structure lr0states =
struct
	open lr0Item
	
	structure state : ORD_KEY = 
	struct 
		open ItemSet
		type ord_key = ItemSet.set
	end

	(* structure t = RedBlackSetFn(state)
	val t1 = t.singleton(closureFull(ItemSet.singleton({lhs = Atom.atom "S", before = [], after = List.map Atom.atom ["E", "$"]})))
	val p = map printItemSet (t.listItems t1) *)

	signature PROXY = 
	sig
	   type proxy
	   type actual
	end

	(* functor Proxy (A : ORD_KEY) = 
	struct
	    type proxy = int
		type actual = A.ord_key
	    val sofar = ref 0
	    (* type proxyMap = ref (map from A.ord_key to int)
	    type reverseMap = ref (map from int -> A.ord_key) *)

	    (* val proxy : actual -> proxy
	    val actual : proxy -> actual *)
	end *)

end