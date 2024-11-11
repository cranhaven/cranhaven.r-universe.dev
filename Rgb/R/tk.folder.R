# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html
# Last update : 2013-08-06 11:45

# Interactive tcltk::tcl-TK directory choosing
tk.folder = function(
		title = "Choose a directory",
		mustexist = TRUE,
		mandatory = TRUE
		)
	{
	# Dialog
	suppressWarnings(folder <- tcltk::tclvalue(tcltk::tkchooseDirectory(title=title, mustexist=mustexist)))
	
	# No folder
	if(mandatory && folder == "") stop("No directory selected")
	
	return(folder)
}

