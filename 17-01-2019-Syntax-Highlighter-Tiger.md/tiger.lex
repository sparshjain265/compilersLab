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

%%

{keywords}	=> (current_pos := yypos - !pos_last_line	; Tokens.KEYWORDS (!current_pos , !lineNum) yytext );
.				=> (current_pos := yypos - !pos_last_line	; Tokens.NORMAL (!current_pos , !lineNum) yytext );
