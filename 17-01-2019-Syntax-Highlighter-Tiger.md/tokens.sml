structure Tokens =
struct
	type position	= int * int
	type token		= string * string
	fun KEYWORDS (i, j) x = (x, "red")
	fun NORMAL (i, j) x = (x, "white")
	fun EOF (i,j) x = ("" , "EOF")
end	