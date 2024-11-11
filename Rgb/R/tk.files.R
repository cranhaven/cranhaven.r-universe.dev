# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html
# Last update : 2013-08-06 11:45

# Interactive tcltk::tcl-TK file choosing in multiple diretories
tk.files <- function(preselection=character(0), multiple=TRUE, parent=NULL, ...) {
	## FUNCTIONS ##
	
	upCommand <- function() {
		toMove <- as.integer(tcltk::tkcurselection(listBox))[1] + 1L
		if(toMove > 1) {
			# Update files
			tmp <- files[ toMove ]
			files[ toMove ] <<- files[ toMove - 1L ]
			files[ toMove - 1L ] <<- tmp
			
			# Update list
			tcltk::tkdelete(listBox, 0, "end")
			for(f in files) tcltk::tkinsert(listBox, "end", f)
			
			# Keep selection
			tcltk::tcl(listBox, "selection", "set", toMove - 2L)
		}
	}
	
	downCommand <- function() {
		toMove <- as.integer(tcltk::tkcurselection(listBox))[1] + 1L
		if(toMove < length(files)) {
			# Update files
			tmp <- files[ toMove ]
			files[ toMove ] <<- files[ toMove + 1L ]
			files[ toMove + 1L ] <<- tmp
			
			# Update list
			tcltk::tkdelete(listBox, 0, "end")
			for(f in files) tcltk::tkinsert(listBox, "end", f)
			
			# Keep selection
			tcltk::tcl(listBox, "selection", "set", toMove)
		}
	}
	
	addCommand <- function() {
		for(f in tk.file(multiple=multiple, mandatory=FALSE, ...)) {
			tcltk::tkinsert(listBox, "end", f)
			files <<- c(files, f)
		}
	}
	
	removeCommand <- function() {
		toRemove <- as.integer(tcltk::tkcurselection(listBox))
		for(i in 1:length(toRemove)) {
			tcltk::tkdelete(listBox, toRemove[i])
			files <<- files[-(toRemove[i]+1L)]
			toRemove[ toRemove > toRemove[i] ] <- toRemove[ toRemove > toRemove[i] ] - 1L
		}
	}
	
	doneCommand <- function() {
		tcltk::tkdestroy(topLevel)
	}
	
	
	## INTERFACE ##
	
	# Top level
	topLevel <- tcltk::tktoplevel(class="Rgb")
	tcltk::tktitle(topLevel) <- "Select files"
	
	# Make slave
	if(!is.null(parent)) {
		tcltk::tcl("wm", "transient", topLevel, parent)
		tcltk::tcl("wm", "withdraw", topLevel)
		tcltk::tcl("wm", "deiconify", topLevel)
	}
	
		# Title
		listTitle <- tcltk::tklabel(parent=topLevel, text="Current file selection")
		tcltk::tkgrid(listTitle, column=1, row=1, pady=c(5,0))

		# List frame
		listFrame <- tcltk::tkframe(parent=topLevel, padx=5, pady=3)
		tcltk::tkgrid(listFrame, column=1, row=2, padx=5, pady=0, sticky="nsew")

			# Scroll bar
			scrollBar <- tcltk::tkscrollbar(parent=listFrame, repeatinterval=5, command=function(...) { tcltk::tkyview(listBox,...) })
			tcltk::tkgrid(scrollBar, column=2, row=1, sticky="nsw", padx=c(1,0), pady=5)

			# List
			listBox <- tcltk::tklistbox(parent=listFrame, height=10, width=70, selectmode="extended", yscrollcommand=function(...) { tcltk::tkset(scrollBar, ...) }, background = "white")
			tcltk::tkgrid(listBox, column=1, row=1, padx=c(0,1), pady=5, sticky="nsew")
			files <- preselection
			for(f in preselection) tcltk::tkinsert(listBox, "end", f)

		# Button frame
		buttonsFrame <- tcltk::tkframe(parent=topLevel, padx=5, pady=3)
		tcltk::tkgrid(buttonsFrame, column=1, row=3, padx=5, pady=5)

			# Add button
			addButton <- tcltk::tkbutton(parent=buttonsFrame, text="Add", width=10, command=addCommand)
			tcltk::tkgrid(addButton, column=1, row=1, padx=5, pady=5)
			
			# Remove button
			removeButton <- tcltk::tkbutton(parent=buttonsFrame, text="Remove", width=10, command=removeCommand)
			tcltk::tkgrid(removeButton, column=2, row=1, padx=5, pady=5)
			
			# Up button
			upButton <- tcltk::tkbutton(parent=buttonsFrame, text="Move up", width=10, command=upCommand)
			tcltk::tkgrid(upButton, column=3, row=1, padx=5, pady=5)
			
			# Down button
			downButton <- tcltk::tkbutton(parent=buttonsFrame, text="Move down", width=10, command=downCommand)
			tcltk::tkgrid(downButton, column=4, row=1, padx=5, pady=5)
			
			# Done button
			doneButton <- tcltk::tkbutton(parent=buttonsFrame, text="Done", width=10, command=doneCommand)
			tcltk::tkgrid(doneButton, column=5, row=1, padx=c(25, 5), pady=5)
		
	# Resizable
	tcltk::tkgrid.columnconfigure(topLevel, 1, weight=1)
	tcltk::tkgrid.columnconfigure(listFrame, 1, weight=1)
	tcltk::tkgrid.rowconfigure(topLevel, 2, weight=1)
	tcltk::tkgrid.rowconfigure(listFrame, 1, weight=1)
	
	# End
	tcltk::tkwait.window(topLevel)
	return(files)
}

