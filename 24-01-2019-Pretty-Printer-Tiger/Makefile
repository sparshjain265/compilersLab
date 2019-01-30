# This rule says that all `.lex.sml` files depend on the corresponding
# `.lex and they can be achieved by running mllex on $<.
%.lex.sml	: %.lex
				mllex $<

# similar for .grm
%.grm.sml	: %.grm
				mlyacc $<

all 			: indent

clean 		: 
				rm -rf *.lex.sml indent

indent		: tokens.sml print.sml error.sml tiger.lex.sml formatter.sml highlight.sml indent.mlb
				mlton indent.mlb

test 			: all
				${CURDIR}/indent < tester