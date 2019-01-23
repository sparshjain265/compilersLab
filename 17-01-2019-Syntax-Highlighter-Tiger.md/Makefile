# This rule says that all `.lex.sml` files depend on the corresponding
# `.lex and they can be achieved by running mllex on $<.
%.lex.sml: %.lex
	mllex $<

all: highlight

clean:
	rm -rf *.lex.sml highlight

highlight: tokens.sml print.sml error.sml tiger.lex.sml formatter.sml highlight.sml highlight.mlb
				mlton highlight.mlb

test : all
		${CURDIR}/highlight < tester