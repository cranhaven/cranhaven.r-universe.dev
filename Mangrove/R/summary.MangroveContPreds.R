summary.MangroveContPreds <- function(object,...){
	class(object) <- "numeric"
	summary.default(object,...)
	}
