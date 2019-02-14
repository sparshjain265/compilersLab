all				:	createGrammar printGrammar nullable printNullable first printFirst follow printFollow

createGrammar	:	grammar.sml createGrammar.sml createGrammar.mlb
					mlton createGrammar.mlb

printGrammar	:	createGrammar printGrammar.sml printGrammar.mlb
					mlton printGrammar.mlb

nullable		:	createGrammar nullable.sml nullable.mlb
					mlton nullable.mlb

printNullable	:	printGrammar nullable printNullable.sml printNullable.mlb
					mlton printNullable.mlb

first			:	nullable first.sml first.mlb
					mlton first.mlb

printFirst		:	printNullable first printFirst.sml printFirst.mlb
					mlton printFirst.mlb

follow			:	first follow.sml follow.mlb
					mlton follow.mlb

printFollow		:	printFirst follow printFollow.sml printFollow.mlb
					mlton printFollow.mlb

test			:	printFollow
					${CURDIR}/printFollow

clean			:
					rm -rf createGrammar printGrammar nullable printNullable first printFirst follow printFollow