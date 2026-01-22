#  #' This allows us to select the weights of the edges
#  #' @export
#  #' @author Aiden Loe
#  #' @title edge.v
#  #' @details This tells us that the value of the edge is dependent on the randomly drawing
#  #' @description This will increase the thickness of the edges as the value increases. Best to keep it to 0 at all times.
#  #' @param edge.value If it is null, the the values of the edge will be randomly drawn.
#  #' @examples
#  #'
#
#
# # randomly draws number 1:10
#  edge.v <- function(edge.value=NULL){
#  if(!is.null(edge.value)){
#  items <- rep(1:10,10)
#  	edge.v <- sample(items, edge.value, replace=TRUE)
#  	return(edge.v)
#  }
# }
#
