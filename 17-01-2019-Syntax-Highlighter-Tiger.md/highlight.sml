structure Highlight =
struct 
	fun highlight filename =
		let val file = TextIO.openIn filename
			fun get _ = TextIO.input file
			val lexer = TigerLex.makeLexer get
	  
			fun format() =
				let val (text,color) = lexer()
		   	
				in	if (color = "red") then print.print_red text
					else print.print_white text; 
					if color = "EOF" then print("\n") else format()
				  end

	   in format();
	  TextIO.closeIn file
	  end

end

