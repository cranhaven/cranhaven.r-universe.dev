print.MangroveTree <-
function(x,...){
	cat("A Mangrove tree")
	if (length(x$getData()$data) > 0){
		cat(" with attached data")
	}
	cat(":\n")
	x$printTree()
}

