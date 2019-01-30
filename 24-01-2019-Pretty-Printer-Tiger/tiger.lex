type lexresult = Tokens.token

type pos 			= int
val current_pos 	= ref 0
val pos_last_line	= ref 1

val lineNum 	= ErrorMsg.lineNum
val linePos 	= ErrorMsg.linePos
fun err(p1,p2)	= ErrorMsg.error p1

val commentNo	= ref 0

fun eof() = let 
					val pos = hd(!linePos) 
				in 
					Tokens.EOF(pos,!lineNum) ""  
				end

fun updateCurrent yypos = (current_pos := yypos - !pos_last_line)
fun updateLine		yypos = (linePos := yypos :: !linePos)

fun incr r = (r := !r + 1)
fun decr	r = (r := !r - 1)

%% 
%structure TigerLex
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
<INITIAL> {newline}									=> (incr lineNum 			; updateLine yypos	; pos_last_line := yypos										; Tokens.NEWLINE		(!current_pos , !lineNum) yytext );
<INITIAL> {startComment}							=> (updateCurrent yypos	; YYBEGIN COMMENT 	; incr commentNo 													; Tokens.COMMENT		(!current_pos , !lineNum) yytext );
<COMMENT> {startComment}							=> (updateCurrent yypos	; incr commentNo 																				; Tokens.COMMENT		(!current_pos , !lineNum) yytext );
<COMMENT> {endComment}								=> (updateCurrent yypos	; decr commentNo 		; if (!commentNo = 0) then (YYBEGIN INITIAL) else ()	; Tokens.COMMENT		(!current_pos , !lineNum) yytext );
<COMMENT> {newline}									=> (incr lineNum 			; updateLine yypos	; pos_last_line := yypos										; Tokens.NEWLINE		(!current_pos , !lineNum) yytext );
<COMMENT> .												=> (updateCurrent yypos																										; Tokens.COMMENT 		(!current_pos , !lineNum) yytext );  
<INITIAL> ("\"")(\\{escapes}|[^\\"])*("\"")	=> (updateCurrent yypos																										; Tokens.STRING		(!current_pos , !lineNum) yytext );
<INITIAL> [-+]?{d}([.]{d})?([eE][-+]?{d})?	=> (updateCurrent yypos																										; Tokens.NUMERIC		(!current_pos , !lineNum) yytext );
<INITIAL> {keywords}									=> (updateCurrent yypos																										; Tokens.KEYWORDS		(!current_pos , !lineNum) yytext );
<INITIAL> {ws}+										=> (updateCurrent yypos																										; Tokens.WHITESPACE	(!current_pos , !lineNum) yytext );
<INITIAL> {symbols}									=> (updateCurrent yypos																										; Tokens.SYMBOLS		(!current_pos , !lineNum) yytext );
<INITIAL> [a-zA-Z_][a-zA-Z0-9_]*					=> (updateCurrent yypos																										; Tokens.IDENTIFIER	(!current_pos , !lineNum) yytext );
<INITIAL> .												=> (updateCurrent yypos																										; Tokens.ILLEGAL		(!current_pos , !lineNum) yytext );
