structure Tokens =
struct
	type position	= int * int
	type token		= string * string
	fun KEYWORDS (i, j) x = (x, "red")
	fun WHITESPACE (i , j) x = (x , "white")
	fun ILLEGAL (i , j) x = (x , "white")
	fun NEWLINE (i , j) x = (x , "white")
	fun EOF (i,j) x = ("" , "EOF")
	fun SYMBOLS (i,j) x = (x , "yellow")
	fun STRING (i,j) x = (x , "blue")
	fun COMMENT (i , j) x = (x , "green")
	fun IDENTIFIER (i , j) x = (x , "cyan")
	fun NUMERIC (i , j) x = (x , "magneta")
end	