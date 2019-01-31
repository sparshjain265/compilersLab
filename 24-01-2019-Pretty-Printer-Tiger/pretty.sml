structure Pretty =
struct

	type Expr = Ast.Expr

	fun	pretty (Ast.Const x) 		= (print((Int.toString x) ^ " "))
		|	pretty (Ast.Op (x, oper, y)) = (pretty x; print((Ast.binOpToString oper) ^ " "); pretty y)

end

(* 
<INITIAL> {newline}									=> (incrementLine ()		; updateLine yypos	; pos_last_line := yypos										; Tokens.NEWLINE		!current_pos yytext );
<INITIAL> {startComment}							=> (updateCurrent yypos	; YYBEGIN COMMENT 	; incr commentNo 													; Tokens.COMMENT		!current_pos yytext );
<COMMENT> {startComment}							=> (updateCurrent yypos	; incr commentNo 																				; Tokens.COMMENT		!current_pos yytext );
<COMMENT> {endComment}								=> (updateCurrent yypos	; decr commentNo 		; if (!commentNo = 0) then (YYBEGIN INITIAL) else ()	; Tokens.COMMENT		!current_pos yytext );
<COMMENT> {newline}									=> (incrementLine ()		; updateLine yypos	; pos_last_line := yypos										; Tokens.NEWLINE		!current_pos yytext );
<COMMENT> .												=> (updateCurrent yypos																										; Tokens.COMMENT 		!current_pos yytext );  
<INITIAL> ("\"")(\\{escapes}|[^\\"])*("\"")	=> (updateCurrent yypos																										; Tokens.STRING		!current_pos yytext );
<INITIAL> [-+]?{d}([.]{d})?([eE][-+]?{d})?	=> (updateCurrent yypos																										; Tokens.NUMERIC		!current_pos yytext );
<INITIAL> {keywords}									=> (updateCurrent yypos																										; Tokens.KEYWORDS		!current_pos yytext );
<INITIAL> {ws}+										=> (updateCurrent yypos																										; Tokens.WHITESPACE	!current_pos yytext );
<INITIAL> {symbols}									=> (updateCurrent yypos																										; Tokens.SYMBOLS		!current_pos yytext );
<INITIAL> [a-zA-Z_][a-zA-Z0-9_]*					=> (updateCurrent yypos																										; Tokens.IDENTIFIER	!current_pos yytext );
<INITIAL> .												=> (updateCurrent yypos																										; Tokens.ILLEGAL		!current_pos yytext ); 
*)
