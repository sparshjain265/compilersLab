# This rule says that all `.lex.sml` files depend on the corresponding
# `.lex and they can be achieved by running mllex on $<.
%.lex.sml	: %.lex
				mllex $<

# similar for .grm
%.grm.sml	: %.grm
				mlyacc $<

all 			: indent

clean 		: 
				rm -rf *.lex.sml *.grm.sml *.grm.sig indent

indent		: indent.sml indent.mlb tiger.grm.sml tiger.lex.sml tigerAst.sml pretty.sml error.sml
				mlton indent.mlb

test 			: all
				${CURDIR}/indent < test.tig