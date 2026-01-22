#' @export
#' @author Aiden Loe
#' @details To ensure that it is a closed loop logic
#' @description Simple check function
#' @param x This check graphs to confirm that it is a closed loop logic
#' @export
#' @title check.graph
#' @examples
#' check.graph(nodeLogic(value = 1, type= "circuit", itemFamily= 1))

check.graph <- function(x){
	#ensure that it is not directed, is not weighted, is not connected
	stopifnot(!is.directed(x), !is.weighted(x), is.connected(x))

	#add column value in the adjcency matrix and divide it by 2. Return logical value (Reminder formula)
	even <- colSums(as.matrix(as_adj(x))) %% 2 == 0
	type <- "ambiguous"
	if(sum(!even) == 0){type <- "circuit"}
	if(sum(!even) == 2){type <-  "trail"}
 return(list(whichuneven = which(!even), type =type))
}

