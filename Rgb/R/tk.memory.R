# Interactive tcl-TK object selection from the global environment
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

tk.memory <- function(
		parent = NULL
	) {
#	### FIX ### silence R CMD CHECK 'NOTE'
#	tableFrame <- NULL
	
	# Find objects in global environment
	drawCalls <- findDrawables()
	
	# Get meta-data
	drawCount <- length(drawCalls)
	drawNames <- character(0)
	drawClasses <- character(0)
	drawIDs <- character(0)
	for(drawCall in drawCalls) {
		var <- eval(parse(text=drawCall), envir=attr(drawCalls, "envir"))
		drawNames <- c(drawNames, var$name)
		drawClasses <- c(drawClasses, class(var))
		drawIDs <- c(drawIDs, sub("^<environment: (.+)>$", "\\1", utils::capture.output(as.environment(var))))
	}
	
	
	output <- character(0)
	
	addCommand <- function() {
		# Get checkbox results
		drawSelected <- logical(0)
		if(drawCount > 0) for(i in 1:drawCount) drawSelected[i] <- as.logical(as.integer(tcltk::tclvalue(selectVarList[[i]])))
		
		# Filter output list
		tmp <- drawCalls[ drawSelected ]
		attr(tmp, "envir") <- attr(drawCalls, "envir")
		output <<- tmp
		
		tcltk::tkdestroy(topLevel)
	}
	
	cancelCommand <- function() {
		tcltk::tkdestroy(topLevel)
	}
	
	
	# Top level
	topLevel <- tcltk::tktoplevel(class="Rgb")
	tcltk::tktitle(topLevel) <- "Rgb - Drawables in memory"
	tcltk::tkgrid.columnconfigure(topLevel, 1, weight=1)
	tcltk::tkgrid.rowconfigure(topLevel, 2, weight=1)

		# Table frame
		tableFrame <- tcltk::tkframe(parent=topLevel)
			
			# Call column
			callLabel <- tcltk::tklabel(parent=tableFrame, text="Call", background="#888888", padx=5)
			callList <- list()
			tcltk::tkgrid(callLabel, column=1, row=1, sticky="nswe", padx=1, pady=1)
			if(drawCount > 0) for(i in 1:drawCount) {
				callList[[i]] <- tcltk::tklabel(parent=tableFrame, text=drawCalls[i], background="#CCCCCC", padx=5, anchor="w")
				tcltk::tkgrid(callList[[i]], column=1, row=i+1, padx=1, pady=1, sticky="nswe")
			}
			
			# Name column
			nameLabel <- tcltk::tklabel(parent=tableFrame, text="Name", background="#888888", padx=5)
			nameList <- list()
			tcltk::tkgrid(nameLabel, column=2, row=1, sticky="nswe", padx=1, pady=1)
			if(drawCount > 0) for(i in 1:drawCount) {
				nameList[[i]] <- tcltk::tklabel(parent=tableFrame, text=drawNames[i], background="#CCCCCC", padx=5)
				tcltk::tkgrid(nameList[[i]], column=2, row=i+1, padx=1, pady=1, sticky="nswe")
			}
			
			# Class column
			classLabel <- tcltk::tklabel(parent=tableFrame, text="Class", background="#888888", padx=5)
			classList <- list()
			tcltk::tkgrid(classLabel, column=3, row=1, sticky="nswe", padx=1, pady=1)
			if(drawCount > 0) for(i in 1:drawCount) {
				classList[[i]] <- tcltk::tklabel(parent=tableFrame, text=drawClasses[i], background="#CCCCCC", padx=5)
				tcltk::tkgrid(classList[[i]], column=3, row=i+1, padx=1, pady=1, sticky="nswe")
			}
			
			# ID column
			idLabel <- tcltk::tklabel(parent=tableFrame, text="ID", background="#888888", padx=5)
			idList <- list()
			tcltk::tkgrid(idLabel, column=4, row=1, sticky="nswe", padx=1, pady=1)
			if(drawCount > 0) for(i in 1:drawCount) {
				idList[[i]] <- tcltk::tklabel(parent=tableFrame, text=drawIDs[i], background="#CCCCCC", padx=5)
				tcltk::tkgrid(idList[[i]], column=4, row=i+1, padx=1, pady=1, sticky="nswe")
			}
			
			# Select column
			selectLabel <- tcltk::tklabel(parent=tableFrame, text="Select", background="#888888", padx=5)
			selectVarList <- list()
			selectWidgetList <- list()
			tcltk::tkgrid(selectLabel, column=5, row=1, sticky="nswe", padx=1, pady=1)
			if(drawCount > 0) for(i in 1:drawCount) {
				selectVarList[[i]] <- tcltk::tclVar("0")
				selectWidgetList[[i]] <- tcltk::tkcheckbutton(parent=tableFrame, variable=selectVarList[[i]])
				tcltk::tkgrid(selectWidgetList[[i]], column=5, row=i+1, padx=1, pady=1)
			}
		
		tcltk::tkgrid(tableFrame, column=1, row=1, sticky="nsew", padx=5, pady=5)
		tcltk::tkgrid.columnconfigure(tableFrame, 1, weight=1)

		# Button frame
		buttonFrame <- tcltk::tkframe(parent=topLevel)
		
			# Action buttons
			addButton <- tcltk::tkbutton(parent=buttonFrame, text="Add object(s)", command=addCommand)
			cancelButton <- tcltk::tkbutton(parent=buttonFrame, text="Cancel", command=cancelCommand)
			tcltk::tkgrid(addButton, column=1, row=1, padx=5, pady=5)
			tcltk::tkgrid(cancelButton, column=2, row=1, padx=5, pady=5)
		
		tcltk::tkgrid(buttonFrame, column=1, row=3, pady=c(10,0))
	
	# End
	tcltk::tkwait.window(topLevel)
	
	return(output)
}

