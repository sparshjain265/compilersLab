structure firstPass = struct
open AST

(* saved data about the program *)

(* refers whether an error has been encountered or not *)
val isError = ref false;

(* refers to current class pointed by 'this' *)
val thisClass = ref "";

(* levels for variables *)
val level = ref 0;
fun nextLevel () = (level := (!level) + 1)
fun prevLevel () = (level := (!level) - 1)

(* list of currently active/allowed (variable, type, level) *)
val variableList : (ID * Type * int) list ref = ref []

(* structure of atom * atom *)
structure funKey : ORD_KEY = 
struct
	type ord_key = Atom.atom * Atom.atom

	fun compare ((x1, y1), (x2, y2)) = 
		case Atom.compare(x1, x2) of
			LESS 	=> LESS
		|	GREATER => GREATER
		|	EQUAL 	=> Atom.compare(y1, y2)								
end

(* structure which maps from funKey ie atom * atom to something *)
structure classFunMap = RedBlackMapFn(funKey)

(* map from (classname, functionName) -> (Return Type, Formal List) *)
val funMap : (Type * Type list) classFunMap.map ref = ref classFunMap.empty 

(* set of classes *)
val classSet : AtomSet.set ref = ref AtomSet.empty

(* map from classname to it's set of functions *)
val classMap : AtomSet.set AtomMap.map ref = ref AtomMap.singleton(Atom.atom "String", AtomSet.empty)

(* function to delete a level (and delete active variables associated with that level) *)
fun deleteLevel L = 
	if List.null (!variableList)

end