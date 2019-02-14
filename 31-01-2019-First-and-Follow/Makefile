all				:	createGrammar printGrammar nullable printNullable

createGrammar	:	grammar.sml createGrammar.sml createGrammar.mlb
					mlton createGrammar.mlb

printGrammar	:	grammar.sml createGrammar.sml printGrammar.sml printGrammar.mlb
					mlton printGrammar.mlb

nullable		:	grammar.sml createGrammar.sml printGrammar.sml nullable.sml nullable.mlb
					mlton nullable.mlb

printNullable	:	grammar.sml createGrammar.sml printGrammar.sml nullable.sml printNullable.sml printNullable.mlb
					mlton printNullable.mlb

test			:	printNullable
					${CURDIR}/printNullable

clean			:
					rm -rf createGrammar printGrammar nullable printNullable