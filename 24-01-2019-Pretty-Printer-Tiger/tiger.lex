type lexresult = Tokens.token

type pos 			= int
val current_pos 	= ref 0;
val pos_last_line	= ref 1;

val lineNum 	= ErrorMsg.lineNum
val linePos 	= ErrorMsg.linePos
fun err(p1,p2)	= ErrorMsg.error p1

fun eof() = let 
					val pos = hd(!linePos) 
				in 
					Tokens.EOF(pos,!lineNum) ""  
				end

%% 
%structure TigerLex

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

%%

<INITIAL> "/*".*"*/"									=> (current_pos := yypos - !pos_last_line															; Tokens.COMMENT		(!current_pos , !lineNum) yytext );
<INITIAL> ("\"")(\\{escapes}|[^\\"])*("\"")	=> (current_pos := yypos - !pos_last_line															; Tokens.STRING		(!current_pos , !lineNum) yytext );
<INITIAL> {newline}									=> (lineNum := !lineNum+1 ; linePos := yypos :: !linePos	; pos_last_line := yypos	; Tokens.NEWLINE		(!current_pos , !lineNum) yytext );
<INITIAL> [-+]?{d}([.]{d})?([eE][-+]?{d})?		=> (current_pos := yypos - !pos_last_line															; Tokens.NUMERIC		(!current_pos , !lineNum) yytext );
<INITIAL> {keywords}									=> (current_pos := yypos - !pos_last_line															; Tokens.KEYWORDS		(!current_pos , !lineNum) yytext );
<INITIAL> {ws}+											=> (current_pos := yypos - !pos_last_line															; Tokens.WHITESPACE	(!current_pos , !lineNum) yytext );
<INITIAL> {symbols}									=> (current_pos := yypos - !pos_last_line															; Tokens.SYMBOLS		(!current_pos , !lineNum) yytext );
<INITIAL> [a-zA-Z_][a-zA-Z0-9_]*					=> (current_pos := yypos - !pos_last_line															; Tokens.IDENTIFIER	(!current_pos , !lineNum) yytext );
<INITIAL> .												=> (current_pos := yypos - !pos_last_line															; Tokens.ILLEGAL		(!current_pos , !lineNum) yytext );
