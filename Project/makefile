%.lex.sml :  %.lex  
			mllex $<

%.grm.sml : %.grm
			mlyacc $<

all       : compiler

clean     : 
			rm -rf *.lex.sml *.grm.sml *.grm.sig *.grm.desc compiler log *.js

compiler	: signatures.sig printColor.sml printer.sml semantic.sml codeGen.sml subJava.grm subJava.grm.sml subJava.lex subJava.lex.sml driver.sml compiler.mlb
				mlton compiler.mlb

test : all 
	 ${CURDIR}/compiler test1.java test1.js