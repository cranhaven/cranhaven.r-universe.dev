# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html
# Last update : 2013-08-06 11:45

# Interactive tcltk::tcl-TK file choosing
tk.file <- function(title="Choose a file", typeNames="All files", typeExt="*", multiple=FALSE, mandatory=TRUE, type=c("open", "save"), initialdir=NULL, parent=NULL) {
	# Checks
	type <- match.arg(type)
	
	# Ignore all warnings (fix for 'X11 BadDrawable')
	on.exit(options(warn=getOption("warn")))
	options(warn=-1)
	
	# Initial directory
	if(is.null(initialdir)) initialdir <- getOption("tk.currentDirectory", default=getwd())
	
	# File filters
	filetypes <- paste(sprintf("{{%s} {%s}}", typeNames, typeExt), collapse=" ")
	
	# Dialog
	if(is.null(parent)) {
		if(type == "open")        { files <- tcltk::tkgetOpenFile(title=title, filetypes=filetypes, multiple=multiple, initialdir=initialdir)
		} else if(type == "save") { files <- tcltk::tkgetSaveFile(title=title, filetypes=filetypes, initialdir=initialdir)
		}
	} else {
		if(type == "open")        { files <- tcltk::tkgetOpenFile(parent=parent, title=title, filetypes=filetypes, multiple=multiple, initialdir=initialdir)
		} else if(type == "save") { files <- tcltk::tkgetSaveFile(parent=parent, title=title, filetypes=filetypes, initialdir=initialdir)
		}
	}
	
	# Get value
	if(!multiple && as.integer(tcltk::tcl("llength", files)) != 0L) { files <- tcltk::tclvalue(files)
	} else                                                          { files <- as.character(files)
	}
	
	# No file
	if(mandatory && length(files) == 0) stop("No file selected")
	
	# Remind working directory
	if(length(files) > 0) options(tk.currentDirectory=dirname(files[1]))
	
	return(files)
}

