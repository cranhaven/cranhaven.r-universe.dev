# Project: BiclustGUI
# 
# Author: lucp8394
###############################################################################




.checkWarnings = function (messages) 
{
	if (getRcmdr("suppress.X11.warnings")) {
		X11.warning <- grep("X11 protocol error|Warning in structure", 
				messages)
		if (length(X11.warning) > 0) {
			messages <- messages[-X11.warning]
		}
		if (length(messages) == 0) 
			Message()
		else if (length(messages) > 10) {
			messages <- c(paste(length(messages), "warnings."), 
					gettextRcmdr("First and last 5 warnings:"), head(messages, 
							5), ". . .", tail(messages, 5))
			Message(message = paste(messages, collapse = "\n"), 
					type = "warning")
		}
		else {
			if (length(grep("warning", messages, ignore.case = TRUE)) > 
					0) 
				Message(message = paste(messages, collapse = "\n"), 
						type = "warning")
			else Message(message = paste(messages, collapse = "\n"), 
						type = "note")
		}
	}
	else {
		if (length(messages) == 0) 
			Message()
		else if (length(messages) > 10) {
			messages <- c(paste(length(messages), "warnings."), 
					gettextRcmdr("First and last 5 warnings:"), head(messages, 
							5), ". . .", tail(messages, 5))
			Message(message = paste(messages, collapse = "\n"), 
					type = "warning")
		}
		else {
			if (length(grep("warning", messages, ignore.case = TRUE)) > 
					0) 
				Message(message = paste(messages, collapse = "\n"), 
						type = "warning")
			else Message(message = paste(messages, collapse = "\n"), 
						type = "note")
		}
	}
	tkfocus(CommanderWindow())
}

.justDoIt2 <- function (command, log = TRUE, rmd = log) 
{
	command <- enc2native(command)
	Message()
	.console.output <- getRcmdr("console.output")
	.output <- OutputWindow()
	if (!.console.output) {
		width <- (as.numeric(tkwinfo("width", .output)) - 2 * 
					as.numeric(tkcget(.output, borderwidth = NULL)) - 
					2)/as.numeric(tkfont.measure(tkcget(.output, font = NULL), 
								"0"))
		eval(parse(text = paste("options(width=", floor(width), 
								")", sep = "")))
	}
	if (!getRcmdr("suppress.X11.warnings")) {
		messages.connection <- file(open = "w+")
		sink(messages.connection, type = "message")
		on.exit({
					sink(type = "message")
					close(messages.connection)
				})
	}
	else messages.connection <- getRcmdr("messages.connection")
	output.connection <- file(open = "w+")
	sink(output.connection, type = "output")
	on.exit({
				if (!.console.output) sink(type = "output")
				close(output.connection)
			}, add = TRUE)
#	if (log) 
#		logger(command, rmd = rmd)
#	else {
#		pushCommand(command)
#		if (rmd) {
#			if (getRcmdr("use.markdown")) 
#				enterMarkdown(command)
#			if (getRcmdr("use.knitr")) 
#				enterKnitr(command)
#		}
#	}
	result <- try(parse(text = paste(command)), silent = TRUE)
	if (class(result)[1] == "try-error") {
		if (rmd) {
			if (getRcmdr("use.markdown")) {
				removeLastRmdBlock()
				putRcmdr("startNewCommandBlock", TRUE)
			}
			if (getRcmdr("use.knitr")) {
				removeLastRnwBlock()
				putRcmdr("startNewKnitrCommandBlock", TRUE)
			}
		}
		Message(message = paste(strsplit(result, ":")[[1]][2]), 
				type = "error")
		if (.console.output) 
			sink(type = "output")
		tkfocus(CommanderWindow())
		return(result)
	}
	else {
		exprs <- result
		result <- NULL
	}
	for (i in seq_along(exprs)) {
		ei <- exprs[i]
		tcl("update")
		result <- try(withVisible(eval(ei, envir = .GlobalEnv)), 
				silent = TRUE)
		if (class(result)[1] == "try-error") {
			if (rmd) {
				if (getRcmdr("use.markdown")) {
					removeLastRmdBlock()
					putRcmdr("startNewCommandBlock", TRUE)
				}
				if (getRcmdr("use.knitr")) {
					removeLastRnwBlock()
					putRcmdr("startNewKnitrCommandBlock", TRUE)
				}
			}
			Message(message = paste(strsplit(result, ":")[[1]][2]), 
					type = "error")
			if (.console.output) 
				sink(type = "output")
			tkfocus(CommanderWindow())
			return(result)
		}
		result <- if (result$visible == FALSE) 
					NULL
				else result$value
		if (!is.null(result)) 
			pushOutput(result)
		if (.isS4object(result)) 
			show(result)
		else print(result)
		.Output <- readLines(output.connection)
		if (length(.Output) > 0 && .Output[length(.Output)] == 
				"NULL") 
			.Output <- .Output[-length(.Output)]
		if (length(.Output) != 0) {
			if (.console.output) {
				out <- .Output
				sink(type = "output")
				for (line in out) cat(paste(line, "\n", sep = ""))
			}
			else {
				for (line in .Output) tkinsert(.output, "end", 
							paste(line, "\n", sep = ""))
				tkyview.moveto(.output, 1)
			}
		}
		else if (.console.output) 
			sink(type = "output")
		if (RExcelSupported()) 
			putRExcel(".rexcel.last.output", .Output)
		.checkWarnings(readLines(messages.connection))
	}
	if (getRcmdr("RStudio")) 
		Sys.sleep(0)
	result
}


#tkwinfo tlctk

.isS4object <- function (object) 
{
	if (getRversion() < "2.4.0") {
		if (length(attr(object, "class")) != 1) 
			return(FALSE)
		!isVirtualClass(getClass(class(object), TRUE))
	}
	else isS4(object)
}
