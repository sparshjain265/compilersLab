structure Highlight =
struct 
	fun highlight filename =
		let val file = TextIO.openIn filename
			fun get _ = TextIO.input file
			val lexer = TigerLex.makeLexer get
	  
			fun format() =
				let val (text,color) = lexer()
		   	
				in	if (color = "red") then print.print_red text
					else if (color = "green") then print.print_green text 
					else if (color = "blue") then print.print_blue text
					else if (color = "yellow") then print.print_yellow text
					else if (color = "white") then print.print_white text
					else if (color = "magneta") then print.print_magneta text
					else if (color = "default") then print.print_default text
					else print.print_cyan text; 
					if color = "EOF" then print("\n") else format()
				  end

	   in format();
	  TextIO.closeIn file
	  end

end

