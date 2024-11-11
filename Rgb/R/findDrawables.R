# Recursive search for 'drawable' inheriting objects in an environment
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

findDrawables <- function(varNames=NA, envir=globalenv()) {
	# Output as character expression
	trackList <- character(0)
	
	# Default : look into environment
	if(length(varNames) == 1 && is.na(varNames)) varNames <- sprintf("`%s`", ls(envir=envir))
		
	for(varName in varNames) {
		var <- eval(parse(text=varName), envir=envir)
		if(inherits(var, "drawable")) {
			# A drawable variable
			trackList <- c(trackList, varName)
		} else if(inherits(var, "drawable.list")) {
			# Elements of a drawable list
			if(var$count > 0) trackList <- c(trackList, sprintf("%s$objects[[%i]]", varName, 1:var$count))
		} else if(is.list(var)) {
			# Recursive call in lists
			trackList <- c(trackList, findDrawables(sprintf("%s[[\"%s\"]]", varName, names(var))))
		}
	}
	
	# Remind environment
	attr(trackList, "envir") <- envir
	
	return(trackList)
}

