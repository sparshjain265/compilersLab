type pos			= int

type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) token

type p 			= int
val current_p 	= ref 0
val pos_last_line	= ref 1
val current_pos 	= ref (1, 0)
val lineNum 	= ErrorMsg.lineNum
val linePos 	= ErrorMsg.linePos
fun err(p1,p2)	= ErrorMsg.error p1

val commentNo	= ref 0

fun eof() = Tokens.EOF(!current_pos)

fun updateCurrent yypos = (current_p := yypos - !pos_last_line; current_pos := (!lineNum, !current_p))
fun updateLine		yypos = (linePos := yypos :: !linePos)
fun incrementLine ()		= (lineNum := !lineNum + 1; current_pos := (!lineNum, !current_p))

fun incr r = (r := !r + 1)
fun decr	r = (r := !r - 1)

%% 
%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS));
%s COMMENT;

ws = [\ \t];

keywords = "array" 
			| "if" | "then" | "else" 
			| "while" | "for" 
			| "to" | "do" 
			| "let" | "in" | "end" 
			| "of" 
			| "break" 
			| "nil" 
			| "function" | "var" | "type" 
			| "import" 
			| "primitive" 
			| "class" 
			| "extend" 
			| "method" 
			| "new";

symbols 	= "," | ":" | ";"
			| "(" | ")"
			| "[" | "]"
			| "{" | "}"
			| "." 
			| "+" | "-" | "*" | "/" 
			| "=" | "<>" 
			| "<" | "<=" 
			| ">" | ">=" 
			| "&" | ":=" | "|" ;

d 			= [0-9]+;

newline	= "\n\r" | "\r\n" | "\r" | "\n";

escapes	= [abfnrtv]
			| "num"
			| "xnum"
			| "\\"
			| "\""
			| "character";

startComment	= "/*";
endComment		= "*/";
commentSymbols = "/*" | "*/";

%%

<INITIAL> {newline}	=> ( lex() );
<INITIAL> {ws}+		=> ( lex() );

<INITIAL> {d}+			=> (Tokens.CONST	(Option.valOf(Int.fromString yytext), !lineNum, !current_p));
<INITIAL> "+"			=> (Tokens.PLUS	(!lineNum, !current_p));
