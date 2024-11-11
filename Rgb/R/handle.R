# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html
# Last update : 2013-08-06 11:45

# Custom silent condition handling function
# - Messages and warnings are silently caught (execution continues)
# - Errors are silently caught (execution stops)
# - NULL can be used to ignore a condition
handle <- function(
		expr,
		messageHandler,
		warningHandler,
		errorHandler
	) {
	toEval <- ""
	
	# Message handler
	if(!missing(messageHandler)) {
		if(is.null(messageHandler))            messageHandler <- function(m){}
		else if (!is.function(messageHandler)) stop("'messageHandler' must be a function or NULL")
		toEval <- sprintf("%s, message=function(m) { messageHandler(m); invokeRestart(\"muffleMessage\") }", toEval)
	}
	
	# Warning handler
	if(!missing(warningHandler)) {
		if(is.null(warningHandler))            warningHandler <- function(w){}
		else if (!is.function(warningHandler)) stop("'warningHandler' must be a function or NULL")
		toEval <- sprintf("%s, warning=function(w) { warningHandler(w); invokeRestart(\"muffleWarning\") }", toEval)
	}
	
	# Error handler
	if(!missing(errorHandler)) {
		if(is.null(errorHandler))            errorHandler <- function(e){}
		else if (!is.function(errorHandler)) stop("'errorHandler' must be a function or NULL")
		toEval <- sprintf("%s, error=function(e) { errorHandler(e); invokeRestart(\"muffleError\") }", toEval)
	}
	
	# Call execution
	eval(parse(text=sprintf("withRestarts(withCallingHandlers({expr}%s), muffleError=\"\")", toEval)))

	invisible(NULL)
}

