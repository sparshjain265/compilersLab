all				:	createGrammar printGrammar nullable printNullable first printFirst follow printFollow ll1 printLL1Entries lr0 slr1 lr1

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

ll1				:	grammar.sml createGrammar.sml nullable.sml first.sml follow.sml ll1.sml ll1.mlb
					mlton ll1.mlb

printLL1Entries	:	grammar.sml createGrammar.sml printGrammar.sml nullable.sml printNullable.sml first.sml printFirst.sml follow.sml printFollow.sml ll1.sml printLL1Entries.sml printLL1Entries.mlb
					mlton printLL1Entries.mlb

lr0				:	grammar.sml createGrammar.sml nullable.sml first.sml follow.sml lr0Item.sml lr0states.sml lr0.sml lr0.mlb
					mlton lr0.mlb

slr1			:	grammar.sml createGrammar.sml nullable.sml first.sml follow.sml lr0Item.sml lr0states.sml slr1.sml slr1.mlb
					mlton slr1.mlb

testLR0			:	lr0
					./lr0

testSLR1		:	slr1
					./slr1

lr1 			:	grammar.sml createGrammar.sml nullable.sml first.sml follow.sml lr1Item.sml lr1states.sml lr1.sml lr1.mlb
					mlton lr1.mlb

testLR1 		:	lr1
					./lr1

test			:	printLL1Entries
					./printLL1Entries

clean			:
					rm -rf createGrammar printGrammar nullable printNullable first printFirst follow printFollow ll1 printLL1Entries lr0 slr1 lr1