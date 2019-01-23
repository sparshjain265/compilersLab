structure Tokens =
struct
	type position						= int * int
	type token							= string * string
	fun KEYWORDS 		(i, j)	x	= (x , "blue"		)
	fun WHITESPACE 	(i, j)	x	= (x , "red"		)
	fun ILLEGAL 		(i, j)	x	= (x , "red"		)
	fun NEWLINE 		(i, j)	x	= (x , "red"		)
	fun SYMBOLS 		(i, j)	x	= (x , "white"		)
	fun STRING 			(i, j)	x	= (x , "yellow"	)
	fun COMMENT 		(i, j)	x	= (x , "green"		)
	fun IDENTIFIER 	(i, j)	x	= (x , "cyan"		)
	fun NUMERIC 		(i, j)	x 	= (x , "magneta"	)
	fun EOF 				(i, j)	x	= ("", "EOF"		)
end