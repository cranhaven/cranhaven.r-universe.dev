# Interactive tcl-TK file conversion
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

tk.convert = function(
		blocking = FALSE
		)
	{
	## FUNCTIONS ##
	
	inputFilesBrowse <- function() {
		inputFiles <<- tk.file(
			title = "Choose the file(s) to convert",
			typeNames = c("Exported refTable objects", "Tab-separated file", "Comma-Separated Value file"),
			typeExt = c(".rdt", ".txt", ".csv"),
			multiple = TRUE,
			mandatory = FALSE,
			type = "open",
			parent = topLevel
		)
		tcltk::tclvalue(inputFilesValue) <- sprintf("%i file(s) selected", length(inputFiles))
		
		if(length(inputFiles) > 0) {
			# Type
			inputFormat <<- sub("^.*(\\.[^\\.]+)$", "\\1", inputFiles)
			inputFormat <<- unique(tolower(inputFormat))
			if(length(inputFormat) > 1) {
				inputFiles <<- character(0)
				inputFormat <<- character(0)
				return(tcltk::tkmessageBox(parent=topLevel, icon="error", type="ok", title="Input error", message="Select input files from the same type"))
			}
			
			# Parameters
			if(inputFormat == ".csv") {
				tcltk::tkconfigure(inputMetaEntry, state="normal")
				tcltk::tkconfigure(inputSepEntry, state="normal")
				tcltk::tkconfigure(inputDecEntry, state="normal")
				tcltk::tkconfigure(inputQuoteEntry, state="normal")
				tcltk::tclvalue(inputSepValue) <- ","
			} else if(inputFormat == ".txt") {
				tcltk::tkconfigure(inputMetaEntry, state="normal")
				tcltk::tkconfigure(inputSepEntry, state="normal")
				tcltk::tkconfigure(inputDecEntry, state="normal")
				tcltk::tkconfigure(inputQuoteEntry, state="normal")
				tcltk::tclvalue(inputSepValue) <- "\\t"
			} else if(inputFormat == ".rdt") {
				tcltk::tkconfigure(inputMetaEntry, state="disabled")
				tcltk::tkconfigure(inputSepEntry, state="disabled")
				tcltk::tkconfigure(inputDecEntry, state="disabled")
				tcltk::tkconfigure(inputQuoteEntry, state="disabled")
			} else {
				return(tcltk::tkmessageBox(parent=topLevel, icon="error", type="ok", title="Input error", message="Unhandled input format (.csv, .txt or .rdt)"))
			}
		} else {
			inputFormat <<- character(0)
		}
	}
	
	outputFormatChange <- function(s) {
		outputFormat <<- sub("^.*\\((.*)\\)$", "\\1", s)
		if(outputFormat == ".csv") {
			# Parameters
			tcltk::tkconfigure(outputMetaEntry, state="normal")
			tcltk::tkconfigure(outputSepEntry, state="normal")
			tcltk::tkconfigure(outputDecEntry, state="normal")
			tcltk::tkconfigure(outputQuoteEntry, state="normal")
			tcltk::tclvalue(outputSepValue) <- ","
		} else if(outputFormat == ".txt") {
			# Parameters
			tcltk::tkconfigure(outputMetaEntry, state="normal")
			tcltk::tkconfigure(outputSepEntry, state="normal")
			tcltk::tkconfigure(outputDecEntry, state="normal")
			tcltk::tkconfigure(outputQuoteEntry, state="normal")
			tcltk::tclvalue(outputSepValue) <- "\\t"
		} else if(outputFormat == ".rdt") {
			# Parameters
			tcltk::tkconfigure(outputMetaEntry, state="disabled")
			tcltk::tkconfigure(outputSepEntry, state="disabled")
			tcltk::tkconfigure(outputDecEntry, state="disabled")
			tcltk::tkconfigure(outputQuoteEntry, state="disabled")
		} else {
			return(tcltk::tkmessageBox(parent=topLevel, icon="error", type="ok", title="Input error", message="Unhandled output format (.csv, .txt or .rdt)"))
		}
	}
	
	convertAction <- function() {
		if(length(inputFiles) == 0) {
			return(tcltk::tkmessageBox(parent=topLevel, icon="error", type="ok", title="Input error", message="Select file(s) to convert"))
		}
		
		# Input parameters
		if(inputFormat == ".csv" || inputFormat == ".txt") {
			inputMeta <- tcltk::tclvalue(inputMetaValue)
			inputMetaRegex <- sprintf("^%s *(.+?) *= *(.+?) *$", inputMeta)
			inputSep <- tcltk::tclvalue(inputSepValue)
			if(inputSep %in% c("\\t", "TAB", "tab")) inputSep <- "\t"
			inputDec <- tcltk::tclvalue(inputDecValue)
			inputQuote <- tcltk::tclvalue(inputQuoteValue)
			if(inputQuote == "" || inputQuote == "NULL") inputQuote <- NULL
		}
		
		# Output parameters
		if(outputFormat == ".csv" || outputFormat == ".txt") {
			outputMeta <- tcltk::tclvalue(outputMetaValue)
			outputMetaRegex <- sprintf("^%s *(.+?) *= *(.+?) *$", outputMeta)
			outputSep <- tcltk::tclvalue(outputSepValue)
			if(outputSep %in% c("\\t", "TAB", "tab")) outputSep <- "\t"
			outputDec <- tcltk::tclvalue(outputDecValue)
			outputQuote <- as.logical(as.integer(tcltk::tclvalue(outputQuoteValue)))
		}
		
		# Process files
		success <- 0L
		for(inputFile in inputFiles) {
			# READ
			if(inputFormat == ".csv" || inputFormat == ".txt") {
				# Import content
				tabularContent <- utils::read.table(inputFile, header=TRUE, sep=inputSep, dec=inputDec, quote=inputQuote, stringsAsFactors=FALSE, comment.char=inputMeta)
				
				# Import metadata
				if(inputMeta != "") {
					metaContent <- scan(inputFile, what="", sep="\n", quote=inputQuote, nlines=50, comment.char="", quiet=TRUE)
					metaContent <- grep(inputMetaRegex, metaContent, value=TRUE)
					metaData <- sub(inputMetaRegex, "\\2", metaContent)
					names(metaData) <- sub(inputMetaRegex, "\\1", metaContent)
					metaData[ metaData == "NA" ] <- NA
				} else {
					metaData <- character(0)
				}
			} else if(inputFormat == ".rdt") {
				# Import content
				content <- readRDT(inputFile)
				if(!is(content, "refTable")) {
					tcltk::tkmessageBox(parent=topLevel, icon="warning", type="ok", title="Input error", message=sprintf("'%s' ignored as it does not contain a refTable-inheriting object", basename(inputFile)))
					break;
				}
				tabularContent <- content$extract()
				
				# Import metadata
				metaData <- character(0)
				fields <- setdiff(names(content$getRefClass()$fields()), names(getRefClass("refTable")$fields()))
				for(fieldName in fields) {
					fieldContent <- content$field(fieldName)
					if(is.atomic(fieldContent) && length(fieldContent) == 1) {
						metaData[ fieldName ] <- as.character(fieldContent)
					}
				}
			}
			
			# FILE NAME
			outputFile <- sprintf("%s%s", sub("\\.[^\\.]+$", "", inputFile), outputFormat)
			
			# WRITE
			if(outputFormat == ".csv" || outputFormat == ".txt") {
				# Write in file
				if(outputMeta != "") cat(sprintf("%s%s=%s\n", outputMeta, names(metaData), metaData), file=outputFile, append=FALSE, sep="")
				suppressWarnings(utils::write.table(tabularContent, file=outputFile, sep=outputSep, dec=outputDec, quote=outputQuote, col.names=TRUE, row.names=FALSE, append=(outputMeta != "")))
			} else if(outputFormat == ".rdt") {
				# Factor conversions
				if("chrom" %in% names(tabularContent) && !is.factor(tabularContent$chrom)) tabularContent$chrom <- factor(tabularContent$chrom)
				if("strand" %in% names(tabularContent) && !is.factor(tabularContent$strand)) tabularContent$strand <- factor(tabularContent$strand)
				
				# Build object
				object <- try(track.table(tabularContent, warn=FALSE), silent=TRUE)
				if(is(object, "try-error")) {
					tcltk::tkmessageBox(parent=topLevel, icon="warning", type="ok", title="Input error", message=sprintf("'%s' ignored as the content produces an invalid track.table object :\n\n%s", basename(inputFile), conditionMessage(attr(object, "condition"))))
					break;
				}
				
				if(length(metaData) > 0) {
					# Fill metadata
					for(fieldName in names(metaData)) {
						fieldClass <- class(object$field(fieldName))
						object$field(fieldName, as(metaData[fieldName], fieldClass))
					}
					
					# Recheck object
					tryResult <- try(object$check(warn=FALSE), silent=TRUE)
					if(is(tryResult, "try-error")) {
						tcltk::tkmessageBox(parent=topLevel, icon="warning", type="ok", title="Input error", message=sprintf("'%s' ignored as the metadata produces an invalid track.table object :\n\n%s", basename(inputFile), conditionMessage(attr(tryResult, "condition"))))
						break;
					}
				}
				
				# Export object
				saveRDT(object, file=outputFile)
			}
			
			# Success counter
			success <- success + 1L
		}
		
		# Finished
		return(tcltk::tkmessageBox(parent=topLevel, icon="info", type="ok", title="Done", message=sprintf("%d/%d file converted", success, length(inputFiles))))
	}


	## INTERFACE ##
	
	# Linux default style
	if(.Platform$OS.type == "unix") try(tcltk::tcl("ttk::style", "theme", "use", "clam"), silent=TRUE)
	
	# Top level
	topLevel <- tcltk::tktoplevel(class="Rgb")
	tcltk::tktitle(topLevel) <- "Rgb - File conversion"
	icon16 <- tcltk::tcl("image", "create", "photo", file=system.file("cghRA_16x16.gif", package="Rgb"))
	icon32 <- tcltk::tcl("image", "create", "photo", file=system.file("cghRA_32x32.gif", package="Rgb"))
	tcltk::tcl("wm", "iconphoto", topLevel, "-default", icon16, icon32)
	
		# Input frame
		inputFrame <- tcltk::ttklabelframe(parent=topLevel, relief="groove", borderwidth=2, text="Input")
			
			# Input file
			inputFiles <- character(0)
			inputFormat <- character(0)
			inputFilesValue <- tcltk::tclVar("0 file(s) selected")
			inputFilesButton <- tcltk::tkbutton(parent=inputFrame, text="Select file(s)", command=inputFilesBrowse, width=18)
			inputFilesEntry <- tcltk::tklabel(parent=inputFrame, textvariable=inputFilesValue, width=28)
			tcltk::tkgrid(inputFilesButton, column=1, row=1, pady=1)
			tcltk::tkgrid(inputFilesEntry, column=2, row=1, pady=1, padx=5, sticky="w")
			
			# Separator
			inputSepValue <- tcltk::tclVar("\\t")
			inputSepLabel <- tcltk::tklabel(parent=inputFrame, text="Column separator", width=20)
			inputSepEntry <- tcltk::tkentry(parent=inputFrame, textvariable=inputSepValue, width=3, justify="center", state="disabled")
			tcltk::tkgrid(inputSepLabel, column=1, row=2, pady=1)
			tcltk::tkgrid(inputSepEntry, column=2, row=2, pady=1)
			
			# Decimal
			inputDecValue <- tcltk::tclVar(".")
			inputDecLabel <- tcltk::tklabel(parent=inputFrame, text="Decimal separator", width=20)
			inputDecEntry <- tcltk::tkentry(parent=inputFrame, textvariable=inputDecValue, width=3, justify="center", state="disabled")
			tcltk::tkgrid(inputDecLabel, column=1, row=3, pady=1)
			tcltk::tkgrid(inputDecEntry, column=2, row=3, pady=1)
			
			# Metadata
			inputMetaValue <- tcltk::tclVar("#")
			inputMetaLabel <- tcltk::tklabel(parent=inputFrame, text="Metadata tag", width=20)
			inputMetaEntry <- tcltk::tkentry(parent=inputFrame, textvariable=inputMetaValue, width=3, justify="center", state="disabled")
			tcltk::tkgrid(inputMetaLabel, column=1, row=4, pady=1)
			tcltk::tkgrid(inputMetaEntry, column=2, row=4, pady=1, padx=5)
			
			# Quote
			inputQuoteValue <- tcltk::tclVar("\"")
			inputQuoteLabel <- tcltk::tklabel(parent=inputFrame, text="Cell quoting", width=20)
			inputQuoteEntry <- tcltk::tkentry(parent=inputFrame, textvariable=inputQuoteValue, width=3, justify="center", state="disabled")
			tcltk::tkgrid(inputQuoteLabel, column=1, row=5, pady=1)
			tcltk::tkgrid(inputQuoteEntry, column=2, row=5, pady=1)
		
		tcltk::tkgrid(inputFrame, column=1, row=1, padx=5, pady=5, sticky="we")
		tcltk::tkgrid.columnconfigure(inputFrame, 3, weight=1)
		
		# Output frame
		outputFrame <- tcltk::ttklabelframe(parent=topLevel, relief="groove", borderwidth=2, text="Output")
			
			# Output format
			outputFormat <- ".csv"
			outputFormatLabel <- tcltk::tklabel(parent=outputFrame, text="File format", width=20)
			outputFormatSpin <- tcltk::tkwidget(outputFrame, "spinbox", values=c("Comma-Separated Value (.csv)", "Tab-Separated file (.txt)", "Track.table object (.rdt)"), wrap=1, width=28, justify="center", command=outputFormatChange)
			tcltk::tkgrid(outputFormatLabel, column=1, row=1, pady=1)
			tcltk::tkgrid(outputFormatSpin, column=2, row=1, pady=1, padx=5)
			
			# Separator
			outputSepValue <- tcltk::tclVar(",")
			outputSepLabel <- tcltk::tklabel(parent=outputFrame, text="Column separator", width=20)
			outputSepEntry <- tcltk::tkentry(parent=outputFrame, textvariable=outputSepValue, width=3, justify="center")
			tcltk::tkgrid(outputSepLabel, column=1, row=2, pady=1)
			tcltk::tkgrid(outputSepEntry, column=2, row=2, pady=1)
			
			# Decimal
			outputDecValue <- tcltk::tclVar(".")
			outputDecLabel <- tcltk::tklabel(parent=outputFrame, text="Decimal separator", width=20)
			outputDecEntry <- tcltk::tkentry(parent=outputFrame, textvariable=outputDecValue, width=3, justify="center")
			tcltk::tkgrid(outputDecLabel, column=1, row=3, pady=1)
			tcltk::tkgrid(outputDecEntry, column=2, row=3, pady=1)
			
			# Metadata
			outputMetaValue <- tcltk::tclVar("#")
			outputMetaLabel <- tcltk::tklabel(parent=outputFrame, text="Metadata tag", width=20)
			outputMetaEntry <- tcltk::tkentry(parent=outputFrame, textvariable=outputMetaValue, width=3, justify="center")
			tcltk::tkgrid(outputMetaLabel, column=1, row=4, pady=1)
			tcltk::tkgrid(outputMetaEntry, column=2, row=4, pady=1, padx=5)
			
			# Quote
			outputQuoteValue <- tcltk::tclVar("0")
			outputQuoteLabel <- tcltk::tklabel(parent=outputFrame, text="Cell quoting", width=20)
			outputQuoteEntry <- tcltk::tkcheckbutton(parent=outputFrame, variable=outputQuoteValue)
			tcltk::tkgrid(outputQuoteLabel, column=1, row=5, pady=1)
			tcltk::tkgrid(outputQuoteEntry, column=2, row=5, pady=1)
			
		tcltk::tkgrid(outputFrame, column=1, row=2, padx=5, pady=5, sticky="we")
		tcltk::tkgrid.columnconfigure(outputFrame, 3, weight=1)
		
		# Process button
		convertButton <- tcltk::tkbutton(parent=topLevel, text="Convert", command=convertAction, width=18)
		tcltk::tkgrid(convertButton, column=1, row=3, padx=10, pady=10)
	
	# Wait for closing
	if(isTRUE(blocking)) tcltk::tkwait.window(topLevel)
}

