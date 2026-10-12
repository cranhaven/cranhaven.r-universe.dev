print.MangroveContPreds <- function(x,...){
	class(x) <- "numeric"
	print.default(x,...)
	}
