# Extracts the argument description from an R help page
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

helpArgument <- function(funName, argName) {
	# Path to the help page
	helpPath <- utils::help(funName, help_type="text")
	if(length(helpPath) == 0) stop("No help page found for this function")
	
	# Content of the help page (Rd class)
	getHelpFile <- utils::getAnywhere(".getHelpFile")[[2]][[1]]
	content <- getHelpFile(helpPath)
	
	# Locate arguments
	section <- which(tolower(sapply(content, attr, "Rd_tag")) == "\\arguments")
	if(length(section) == 0) stop("No 'arguments' section found in the help page")
	items <- which(tolower(sapply(content[[section]], attr, "Rd_tag")) == "\\item")
	if(length(items) == 0) stop("No 'item' section found in the 'arguments' section")
	
	# Locate targetted argument
	position <- NA
	for(i in items) {
		if(attr(content[[section]][[i]][[1]][[1]],"Rd_tag") != "\\dots" && content[[section]][[i]][[1]][[1]] == argName) {
			position <- i
		}
	}
	if(is.na(position)) stop("Argument not found in the help page")
	
	# Clean argument description
	x <- unlist(content[[section]][[position]])[-1]
	x <- paste(x, collapse="")
	x <- sub("^[[:space:]]+", "", x)
	x <- sub("[[:space:]]+$", "", x)
	
	return(x)
}
