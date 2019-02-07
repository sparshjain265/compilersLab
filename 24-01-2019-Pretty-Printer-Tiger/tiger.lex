type pos			= int

type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) token

val pos_last_line	= ref 1
val current_pos 	= ref 0
val lineNum 	= ErrorMsg.lineNum
val linePos 	= ErrorMsg.linePos
fun err(p1,p2)	= ErrorMsg.error p1

val commentNo	= ref 0

fun eof() = Tokens.EOF(!lineNum, !current_pos)

fun incr r = (r := !r + 1)
fun decr	r = (r := !r - 1)

fun updateCurrent yypos = (current_pos := yypos - !pos_last_line)
fun updateLine		yypos = (linePos := yypos :: !linePos; pos_last_line := yypos)
fun incrementLine ()		= incr lineNum

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

<INITIAL> {newline}	=> (incrementLine ()		; updateLine yypos	; lex());
<INITIAL> {d}+			=> (updateCurrent yypos ;	Tokens.CONST	(Option.valOf(Int.fromString yytext), !lineNum, !current_pos));
<INITIAL> {ws}+		=> (updateCurrent yypos	; lex());
<INITIAL> "+"			=> (updateCurrent yypos	;	Tokens.PLUS	(!lineNum, !current_pos));
<INITIAL> "-"			=> (updateCurrent yypos	;	Tokens.MINUS	(!lineNum, !current_pos));
<INITIAL> "*"			=> (updateCurrent yypos	;	Tokens.MUL	(!lineNum, !current_pos));
<INITIAL> "/"			=> (updateCurrent yypos	;	Tokens.DIV	(!lineNum, !current_pos));
<INITIAL> "("			=> (updateCurrent yypos	;	Tokens.LPAREN (!lineNum, !current_pos));
<INITIAL> ")"			=> (updateCurrent yypos	;	Tokens.RPAREN (!lineNum, !current_pos));
<INITIAL> [a-zA-Z_][a-zA-Z0-9_]*	=> (updateCurrent yypos	; Tokens.VAR	(yytext, !lineNum, !current_pos));
