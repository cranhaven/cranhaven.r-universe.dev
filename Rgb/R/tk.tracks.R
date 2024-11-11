# Interactive tcl-TK track selection and edition
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

tk.tracks <- function(
		drawables = drawable.list(),
		parent = NULL
	) {
	### FIX ### silence R CMD CHECK 'NOTE'
	tableFrame <- NULL
	
	# Currently selected track
	selectVar <- tcltk::tclVar()
	
	addFileCommand <- function() {
		# Select files
		newTrackFiles <- tk.file(
			title = "Choose a drawable R object",
			typeNames = c("RefTable-inheriting object", "Custom R drawable object"),
			typeExt = c("*.rdt", "*.rds"),
			multiple = TRUE,
			mandatory = FALSE,
			type = "open",
			parent = topLevel
		)
		
		# Process files
		for(file in newTrackFiles) drawables$add(file)
		
		# Update selection
		tcltk::tclvalue(selectVar) <- as.integer(drawables$count)
		
		# Update interface
		refreshTable(tableFrame)
	}
	
	addObjectCommand <- function() {
		# Interactive selection of drawables in R global environment
		varCalls <- tk.memory(parent=topLevel)
		
		# Process objects
		for(varCall in varCalls) {
			var <- eval(parse(text=varCall), envir=attr(varCalls, "envir"))
			drawables$add(file=NA, track=var)
		}
		
		# Update selection
		tcltk::tclvalue(selectVar) <- as.integer(drawables$count)
		
		# Update interface
		refreshTable(tableFrame)
	}
	
	editCommand <- function() {
		# Selected track
		selection <- as.integer(tcltk::tclvalue(selectVar))
		if(is.na(selection)) selection <- 1L
		
		# Call
		drawables$fix.param(selection=selection, parent=topLevel)
	}
	
	upCommand <- function() {
		# Index of selected track
		toMove <- as.integer(tcltk::tclvalue(selectVar))
		
		# Call
		drawables$moveUp(toMove)
		
		# Update selection
		tcltk::tclvalue(selectVar) <- as.integer(tcltk::tclvalue(selectVar)) - 1L
		
		# Update interface
		refreshTable(tableFrame)
	}
	
	downCommand <- function() {
		# Index of selected track
		toMove <- as.integer(tcltk::tclvalue(selectVar))
		
		# Call
		drawables$moveDown(toMove)
			
		# Update selection
		tcltk::tclvalue(selectVar) <- as.integer(tcltk::tclvalue(selectVar)) + 1L
		
		# Update interface
		refreshTable(tableFrame)
	}
	
	removeCommand <- function() {
		# Index of selected track
		toRemove <- as.integer(tcltk::tclvalue(selectVar))
		
		# Call
		drawables$remove(toRemove)
			
		# Update selection if last
		if(toRemove == drawables$count + 1L) tcltk::tclvalue(selectVar) <- drawables$count
		
		# Update interface
		refreshTable(tableFrame)
	}
	
	doneCommand <- function() {
		tcltk::tkdestroy(topLevel)
	}
	
	hiddenVarList <- list()
	refreshTable <- function(currentFrame=NULL) {
		# Table frame
		tableFrame <- tcltk::tkframe(parent=topLevel)
			
			# Path column
			pathLabel <- tcltk::tklabel(parent=tableFrame, text="File", background="#888888", padx=5)
			pathList <- list()
			tcltk::tkgrid(pathLabel, column=1, row=1, sticky="nswe", padx=1, pady=1)
			if(drawables$count > 0) for(i in 1:drawables$count) {
				pathList[[i]] <- tcltk::tklabel(parent=tableFrame, text=drawables$get(i, "files"), background="#CCCCCC", padx=5, anchor="w")
				tcltk::tkgrid(pathList[[i]], column=1, row=i+1, padx=1, pady=1, sticky="nswe")
			}
			
			# Name column
			nameLabel <- tcltk::tklabel(parent=tableFrame, text="Name", background="#888888", padx=5)
			nameList <- list()
			tcltk::tkgrid(nameLabel, column=2, row=1, sticky="nswe", padx=1, pady=1)
			if(drawables$count > 0) for(i in 1:drawables$count) {
				nameList[[i]] <- tcltk::tklabel(parent=tableFrame, text=drawables$get(i)$name, background="#CCCCCC", padx=5)
				tcltk::tkgrid(nameList[[i]], column=2, row=i+1, padx=1, pady=1, sticky="nswe")
			}
			
			# Class column
			classLabel <- tcltk::tklabel(parent=tableFrame, text="Class", background="#888888", padx=5)
			classList <- list()
			tcltk::tkgrid(classLabel, column=3, row=1, sticky="nswe", padx=1, pady=1)
			if(drawables$count > 0) for(i in 1:drawables$count) {
				classList[[i]] <- tcltk::tklabel(parent=tableFrame, text=drawables$get(i, "classes"), background="#CCCCCC", padx=5)
				tcltk::tkgrid(classList[[i]], column=3, row=i+1, padx=1, pady=1, sticky="nswe")
			}
			
			# Hidden column
			hiddenLabel <- tcltk::tklabel(parent=tableFrame, text="Hidden", background="#888888", padx=5)
			hiddenWidgetList <- list()
			tcltk::tkgrid(hiddenLabel, column=4, row=1, sticky="nswe", padx=1, pady=1)
			if(drawables$count > 0) for(i in 1:drawables$count) {
				hiddenVarList[[i]] <<- tcltk::tclVar(as.integer(drawables$get(i, "hidden")))
				hiddenWidgetList[[i]] <- tcltk::tkcheckbutton(parent=tableFrame, variable=hiddenVarList[[i]])
				tcltk::tkgrid(hiddenWidgetList[[i]], column=4, row=i+1, padx=1, pady=1)
			}
			
			# Select column
			selectLabel <- tcltk::tklabel(parent=tableFrame, text="Action", background="#888888", padx=5)
			selectList <- list()
			tcltk::tkgrid(selectLabel, column=5, row=1, sticky="nswe", padx=1, pady=1)
			if(drawables$count > 0) for(i in 1:drawables$count) {
				selectList[[i]] <- tcltk::tkradiobutton(parent=tableFrame, variable=selectVar, value=i)
				tcltk::tkgrid(selectList[[i]], column=5, row=i+1, padx=1, pady=1)
			}
		
		tcltk::tkgrid(tableFrame, column=1, row=1, sticky="nsew", padx=5, pady=5)
		tcltk::tkgrid.columnconfigure(tableFrame, 1, weight=1)
	}
	
	
	
	# Top level
	topLevel <- tcltk::tktoplevel(class="Rgb")
	tcltk::tktitle(topLevel) <- "Rgb - Track management"
	tcltk::tkgrid.columnconfigure(topLevel, 1, weight=1)
	tcltk::tkgrid.rowconfigure(topLevel, 2, weight=1)

		refreshTable()

		# Button frame
		buttonFrame <- tcltk::tkframe(parent=topLevel)
		
			# Action buttons
			addFileButton <- tcltk::tkbutton(parent=buttonFrame, text="Add from file", command=addFileCommand)
			addObjectButton <- tcltk::tkbutton(parent=buttonFrame, text="Add from memory", command=addObjectCommand)
			doneButton <- tcltk::tkbutton(parent=buttonFrame, text="Done", command=doneCommand)
			editButton <- tcltk::tkbutton(parent=buttonFrame, text="Edit", command=editCommand)
			upButton <- tcltk::tkbutton(parent=buttonFrame, text="Move up", command=upCommand)
			downButton <- tcltk::tkbutton(parent=buttonFrame, text="Move down", command=downCommand)
			deleteButton <- tcltk::tkbutton(parent=buttonFrame, text="Remove", command=removeCommand)
			tcltk::tkgrid(addFileButton, column=1, row=1, padx=5, pady=5)
			tcltk::tkgrid(addObjectButton, column=2, row=1, padx=5, pady=5)
			tcltk::tkgrid(doneButton, column=3, row=1, padx=5, pady=5)
			tcltk::tkgrid(editButton, column=5, row=1, padx=5, pady=5)
			tcltk::tkgrid(upButton, column=6, row=1, padx=5, pady=5)
			tcltk::tkgrid(downButton, column=7, row=1, padx=5, pady=5)
			tcltk::tkgrid(deleteButton, column=9, row=1, padx=5, pady=5)
		
		tcltk::tkgrid(buttonFrame, column=1, row=3, sticky="nsew", pady=c(10,0))
		tcltk::tkgrid.columnconfigure(buttonFrame, 4, weight=1)
		tcltk::tkgrid.columnconfigure(buttonFrame, 8, weight=1)
	
	# End
	tcltk::tkwait.window(topLevel)
	
	# Update 'hidden' status
	if(drawables$count > 0) for(i in 1:drawables$count) drawables$hidden[i] <- as.logical(as.integer(tcltk::tclvalue(hiddenVarList[[i]])))
	
	return(drawables)
}

