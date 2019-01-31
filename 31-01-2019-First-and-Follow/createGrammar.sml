val t = addSymbol "a"
val t = addSymbol "b"
val t = addToken "S"
val t = addToken "B"
val t = addRule "S" [["a", "B"]]
val t = addRule "B" [["b"]]

val p = printSymbols();
val p = printTokens();
val p = printRules();