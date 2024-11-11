# Interactive tcl-TK 'drawable' object edition
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

tk.edit = function(
		drawables,
		selection = 1L,
		parent = NULL	
		)
	{
	# Checks
	if(!is(drawables, "drawable.list")) stop("'drawables' must be a 'drawable.list' object")
	if(drawables$count == 0)            stop("'drawables' is empty")
	
	
	
	## FUNCTIONS ##
	
	# Index of the selected drawable object, currently edited
	index <- selection
	
	trackComboSelect <- function() {
		# Update index
		index <<- as.integer(tcltk::tcl(trackCombo, "current")) + 1L
		
		# Refresh combo lists from tracks
		refreshSaveButton()
		refreshParams()
		refreshContent()
		refreshHelp()
	}
	
	paramComboSelect <- function() {
		# Refresh combo lists from tracks
		refreshContent()
		refreshHelp()
	}
	
	refreshSaveButton <- function() {
		# "Save in file" button disabled when no file path is provided
		if(is.na(drawables$get(index, "files"))) { tcltk::tkconfigure(fileButton, state="disabled")
		} else                                   { tcltk::tkconfigure(fileButton, state="normal")
		}
	}
	
	refreshParams <- function() {
		# Remind current selection
		current <- tcltk::tkget(paramCombo)
		
		# Fill with parameter names
		params <- names(drawables$get(index)$defaultParams())
		tcltk::tkconfigure(paramCombo, values=sort(params))
		
		# Reset current selection, if possible
		if(tcltk::tclvalue(current) == "") { tcltk::tkset(paramCombo, sort(params)[1])
		} else                             { tcltk::tkset(paramCombo, current)
		}
	}
	
	refreshHelp <- function() {
		# Update help
		argName <- tcltk::tclvalue(tcltk::tkget(paramCombo))
		if(argName == "drawFun") {
			# 'drawFun' special handling
			titleText <- "drawFun"
			contentText <- "The name of the R function that will handle the plotting. Rgb provides \"draw.boxes\", \"draw.density\", \"draw.hist\", \"draw.pileup\", \"draw.points\", \"draw.seq\" and \"draw.steps\", but custom functions defined in the R global environment can be used as well."
		} else if(argName == "height") {
			# 'height' special handling
			titleText <- "height"
			contentText <- "Height of the track, as handled by layout(). Can be a fixed height (e.g. \"3 cm\") or a numeric weight for remaining space sharing (tracks with an height of 2 will be twice as large as tracks with an height of 1, and so on)."
		} else {
			# Other arguments
			drawFun <- drawables$get(index)$getParam("drawFun")
			if(argName %in% names(formals("draw.bg")))      { funName <- "draw.bg"
			} else if(argName %in% names(formals(drawFun))) { funName <- drawFun
			} else                                          { funName <- NA
			}
			if(is.na(funName)) {
				titleText <- sprintf("NOT USED - %s", argName)
				contentText <- "This parameter has no effect with the current drawing function."
			} else {
				titleText <- sprintf("%s - %s", funName, argName)
				contentText <- helpArgument(funName, argName)
			}
		}
		
		# Apply update
		tcltk::tkconfigure(helpFrame, text=titleText)
		tcltk::tclvalue(helpMessageVariable) <- contentText
	}
	
	refreshContent <- function() {
		if(tcltk::tclvalue(tcltk::tkget(paramCombo)) != "") {
			# New content
			newContent <- drawables$get(index)$getParam(tcltk::tclvalue(tcltk::tkget(paramCombo)))
			newText <- paste(deparse(newContent), collapse="\n")
			
			# Update text
			tcltk::tkdelete(contentText, "0.0", "end")
			tcltk::tkinsert(contentText, "0.0", newText)
		
			# Default or custom
			if(tcltk::tclvalue(tcltk::tkget(paramCombo)) %in% names(drawables$get(index)$parameters)) { tcltk::tkconfigure(contentText, background=defaultBackground)
			} else                                                                                    { tcltk::tkconfigure(contentText, background="#EEEEEE")
			}
		}
	}
	
	processText <- function() {
		# For failures
		content <- NULL
		value <- tcltk::tclvalue(tcltk::tkget(contentText, "0.0", "end"))
		
		if(gsub("\\s", "", value) != "") {
			# Parse expression
			handle(
				expr = {
					expr <- parse(text=value)
					content <- eval(expr)
					content <- list(content)
				},
				warningHandler = function(w) {
					tcltk::tkmessageBox(
						parent = topLevel,
						icon = "warning",
						type = "ok",
						title = sprintf("'%s' warning", as.character(conditionCall(w)[[1]])),
						message = conditionMessage(w)
					)
				},
				errorHandler = function(e) {
					tcltk::tkmessageBox(
						parent = topLevel,
						icon = "error",
						type = "ok",
						title = sprintf("'%s' error", as.character(conditionCall(e)[[1]])),
						message = conditionMessage(e)
					)
				}
			)
		} else {
			# Empty expression
			content <- list()
		}
		
		return(content)
	}
	
	update <- function(updateFile) {
		# Selected track (names can be ambiguous)
		trackIndex <- as.integer(tcltk::tcl(trackCombo, "current")) + 1L
		
		content <- processText()
		if(is.list(content)) {
			# Update object
			if(length(content) == 1) { reset <- FALSE; drawables$get(index)$setParam(name=tcltk::tclvalue(tcltk::tkget(paramCombo)), value=content[[1]])
			} else                   { reset <- TRUE;  drawables$get(index)$setParam(name=tcltk::tclvalue(tcltk::tkget(paramCombo)))
			}
			
			# Update param list if drawFun is updated
			if(tcltk::tclvalue(tcltk::tkget(paramCombo)) == "drawFun") refreshParams()
			
			# Update file
			if(isTRUE(updateFile)) {
				if(!is.na(drawables$get(index, "files"))) {
					if(grepl("\\.rdt$", drawables$get(index, "files"), ignore.case=TRUE))        { saveRDT(drawables$get(index), file=drawables$get(index, "files"))				
					} else if(grepl("\\.rds$", drawables$get(index, "files"), ignore.case=TRUE)) { saveRDS(drawables$get(index), file=drawables$get(index, "files"))
					} else                                                                       { tcltk::tkmessageBox(parent=topLevel, icon="error", type="ok", title="Unknown file extension", message="Can not find a proper way to save the drawable object in this file (must be a .rds or .rdt file)")
					}
				}
			}
			
			# Confirmation
			if(updateFile) {
				if(!is.na(drawables$get(index, "files"))) {
					if(reset) { tcltk::tkmessageBox(parent=topLevel, icon="info", type="ok", title="Reset", message=sprintf("Parameter value reset to default in memory and in file :\n\n%s", drawables$get(index, "files")))
					} else    { tcltk::tkmessageBox(parent=topLevel, icon="info", type="ok", title="Updated", message=sprintf("Parameter value updated in memory and in file :\n\n%s", drawables$get(index, "files")))
					}
				}
			} else {
				if(reset) { tcltk::tkmessageBox(parent=topLevel, icon="info", type="ok", title="Reset", message="Parameter value reset to default in memory")
				} else    { tcltk::tkmessageBox(parent=topLevel, icon="info", type="ok", title="Updated", message="Parameter value updated in memory")
				}
			}
		}
	}
	
	memoryAction <- function() {
		update(updateFile=FALSE)
	}
	
	fileAction <- function() {
		update(updateFile=TRUE)
	}
	
	closeAction <- function() {
		tcltk::tkdestroy(topLevel)
	}
	


	## INTERFACE ##

	# Linux default style
	if(.Platform$OS.type == "unix") try(tcltk::tcl("ttk::style", "theme", "use", "clam"), silent=TRUE)
	
	# Top level
	topLevel <- tcltk::tktoplevel(class="Rgb")
	tcltk::tktitle(topLevel) <- "Rgb - Drawing parameters"
	icon16 <- tcltk::tcl("image", "create", "photo", file=system.file("cghRA_16x16.gif", package="Rgb"))
	icon32 <- tcltk::tcl("image", "create", "photo", file=system.file("cghRA_32x32.gif", package="Rgb"))
	tcltk::tcl("wm", "iconphoto", topLevel, "-default", icon16, icon32)
	
	# Make slave
	if(!is.null(parent)) {
		tcltk::tcl("wm", "transient", topLevel, parent)
		tcltk::tcl("wm", "withdraw", topLevel)
		tcltk::tcl("wm", "deiconify", topLevel)
	}
	
		# Track frame
		trackFrame <- tcltk::tkframe(parent=topLevel)
			
			# Track combo initialization (FIX)
			if(drawables$count == 1) { trackComboInit <- sprintf("\"%s\"", drawables$names)
			} else                   { trackComboInit <- drawables$names
			}
			
			# Track combo
			trackCombo <- tcltk::ttkcombobox(parent=trackFrame, values=trackComboInit, width=25, justify="center", state="readonly")
			tcltk::tcl(trackCombo, "set", drawables$names[index])
			tcltk::tkgrid(trackCombo, column=1, row=1, padx=5, pady=5)
			
			# Parameter combo
			paramCombo <- tcltk::ttkcombobox(parent=trackFrame, values=character(0), width=12, justify="center", state="readonly")
			tcltk::tkgrid(paramCombo, column=2, row=1, padx=5, pady=5)
			
		tcltk::tkgrid(trackFrame, column=1, row=1)
		
		# Help frame
		helpFrame <- tcltk::ttklabelframe(parent=topLevel, relief="groove", text="")
		
			# Help text
			helpMessageVariable <- tcltk::tclVar("")
			helpMessage <- tcltk::tkmessage(parent=helpFrame, textvariable=helpMessageVariable, width=350)
			tcltk::tkgrid(helpMessage, column=1, row=1, padx=8, pady=8, sticky="nsew")
		
		tcltk::tkgrid(helpFrame, column=1, padx=10, pady=5, row=2)
		
		# Content text
		contentText <- tcltk::tktext(parent=topLevel, width=50, height=15)
		tcltk::tkgrid(contentText, column=1, row=3, padx=5, pady=5, sticky="nsew")
		defaultBackground <- as.character(tcltk::tkcget(contentText, "-background"))
		
		# Button frame
		buttonFrame <- tcltk::tkframe(parent=topLevel)
			
			# Memory button
			memoryButton <- tcltk::tkbutton(parent=buttonFrame, text="Save in memory", command=memoryAction, width=15)
			tcltk::tkgrid(memoryButton, column=1, row=1, padx=5, pady=5)
			
			# File button
			fileButton <- tcltk::tkbutton(parent=buttonFrame, text="Save in file", command=fileAction, width=15)
			tcltk::tkgrid(fileButton, column=2, row=1, padx=5, pady=5)
			
			# Close button
			closeButton <- tcltk::tkbutton(parent=buttonFrame, text="Close and discard", command=closeAction, width=15)
			tcltk::tkgrid(closeButton, column=3, row=1, padx=5, pady=5)
		
		tcltk::tkgrid(buttonFrame, column=1, row=4)
		
	# Resizable
	tcltk::tkgrid.columnconfigure(topLevel, 1, weight=1)
	tcltk::tkgrid.rowconfigure(topLevel, 2, weight=1)


	## LAUNCH ##
	
	# Erase combobox text selection
	tcltk::tkbind(trackCombo, "<<ComboboxSelected>>", trackComboSelect)
	tcltk::tkbind(paramCombo, "<<ComboboxSelected>>", paramComboSelect)
	
	# Refresh combo lists from tracks
	refreshSaveButton()
	refreshParams()
	refreshContent()
	refreshHelp()
	
	# End
	tcltk::tkwait.window(topLevel)
	
	invisible(TRUE)
}

