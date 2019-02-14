all				:	createGrammar printGrammar nullable printNullable first printFirst follow printFollow

createGrammar	:	grammar.sml createGrammar.sml createGrammar.mlb
					mlton createGrammar.mlb

printGrammar	:	grammar.sml createGrammar.sml printGrammar.sml printGrammar.mlb
					mlton printGrammar.mlb

nullable		:	grammar.sml createGrammar.sml nullable.sml nullable.mlb
					mlton nullable.mlb

printNullable	:	grammar.sml createGrammar.sml printGrammar.sml nullable.sml printNullable.sml printNullable.mlb
					mlton printNullable.mlb

first			:	grammar.sml createGrammar.sml nullable.sml first.sml first.mlb
					mlton first.mlb

printFirst		:	grammar.sml createGrammar.sml printGrammar.sml nullable.sml printNullable.sml first.sml printFirst.sml printFirst.mlb
					mlton printFirst.mlb

follow			:	grammar.sml createGrammar.sml nullable.sml first.sml follow.sml follow.mlb
					mlton follow.mlb

printFollow		:	grammar.sml createGrammar.sml printGrammar.sml nullable.sml printNullable.sml first.sml printFirst.sml follow.sml printFollow.sml printFollow.mlb
					mlton printFollow.mlb

test			:	printFollow
					${CURDIR}/printFollow

clean			:
					rm -rf createGrammar printGrammar nullable printNullable first printFirst follow printFollow