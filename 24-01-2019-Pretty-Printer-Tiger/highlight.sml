val fileN		= Option.valOf (TextIO.inputLine TextIO.stdIn)
val fileName	= String.substring(fileN, 0, size fileN - 1)
val myOut		= Formatter.formatter fileName