%.lex.sml :  %.lex  
			mllex $<

%.grm.sml : %.grm
			mlyacc $<

all       : compiler

clean     : 
			rm -rf *.lex.sml *.grm.sml *.grm.sig *.grm.desc compiler

compiler	: subJava.grm subJava.grm.sml subJava.lex subJava.lex.sml compiler.mlb
				mlton compiler.mlb

test : all 
	 ${CURDIR}/compiler test1.java