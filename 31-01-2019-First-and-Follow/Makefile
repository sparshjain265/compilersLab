all				:	createGrammar printGrammar

createGrammar	:	grammar.sml createGrammar.sml
					mlton createGrammar.mlb

printGrammar	:	grammar.sml createGrammar.sml printGrammar.sml
					mlton printGrammar.mlb

nullable		:	grammar.sml createGrammar.sml printGrammar.sml nullable.sml
					mlton nullable.mlb

test			:	nullable
					${CURDIR}/nullable

clean			:
					rm -rf createGrammar printGrammar