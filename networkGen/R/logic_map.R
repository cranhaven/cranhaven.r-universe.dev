
#' @export
#' @import igraph
#' @param x This is taken from the logic derived using the igraph package.
#' @param base.colour This is the colour of all the nodes if no colour is specified
#' @param start.colour This is the colour of the first node
#' @param end.colour This is the colour of the last node
#' @param newValue This is the value of the number of nodes from the logic
#' @param names If names=NUll then use default names in the package
#' @param default.colour If TRUE, then the colours of the node will not change. If FALSE, the colours of the node will change.
#' @param no.label If no.labels is TRUE, then it will not print the names in the nodes. If FALSE, then it will be numeric values.
#' @description This function is used to change the node display.
#' @details This functions is embedded with \code{check.graph}, \code{edge.v}, \code{colour_display}. For example, you can use this function to add in names inside the node, or change the width of the edge, or to include the labels in the nodes given by the names arg. Generally, it is used for the assisting in the design of the network maze
#' @return The map item based on the logic of the igraph package
#' @author Aiden Loe
#' @details
#' This allow us to create a map with close looped form
#' @title Logic Map
#' @examples
#' logic <- nodeLogic(value = 8, type= "circuit", itemFamily= 1)
#' names <- c('a','b','c','d','e','f','g')
#' logicMap(logic, no.label=FALSE, names=names)

### Trails and Circuits
logicMap <- function(x ,base.colour, start.colour, end.colour, names=NULL, newValue, default.colour=TRUE, no.label=FALSE) {
	#Check graph
	cg <- check.graph(x)
	#Stop if it's neither class
	# %in% checks for class within 'selected' column
	stopifnot(cg$type %in% c("circuit", "trail"))

		#Check colour and add colour
		if(!default.colour){
		V(x)$color <- colour_display(base.colour)$chosen

		#If circuit class
			if(cg$type == "circuit") V(x)$color[1] <- colour_display(base.colour=base.colour, start.colour)$start.node
		#If trail class
			if(cg$type == "trail"){
				V(x)$color[cg$whichuneven[1]] <- colour_display(base.colour=base.colour, start.colour)$start.node
				V(x)$color[cg$whichuneven[2]] <- colour_display(base.colour=base.colour, start.colour=start.colour, end.colour)$end.node
			}
		}else {
				y=NULL
			  }

	# Labelling
	#E(x)$weight <- edge.v(edge.value)
	if(!no.label){
	newValue <-  length(V(x)$name)
	V(x)$name = v.labels(names=names, newValue = newValue)$d.names
     }else{
     	V(x)$name= V(x)$name
     	  }
	return(x)
}



# #' Map items that that does require a check
# #' @param x This is taken from the logic derived using the igraph package.
# #' @param base.colour This is the colour of all the nodes if no colour is specified
# #' @param start.colour This is the colour of the first node
# #' @param end.colour This is the colour of the last node
# #' @param newValue This is the value of the number of nodes from the logic
# #' @param names If names=NUll then use default names in the package
# #' @param edge.value This depends on the width of the edge
# #' @details This functions is embedded with \code{check.graph}, \code{edge.v}, \code{colour_display}
# #' @return The map item based on the logic of the igraph package
# #' @export
# #' @author Aiden Loe
# #' @details Map items that that does require a check
# #' This allow us to create a map with close looped form
# #' @title g.map
#
# g.map <- function(x ,base.colour, start.colour, end.colour, names=NULL, newValue, default.colour=TRUE, edge.value=0, no.label=TRUE) {
#
# 		if(!default.colour){
# 		V(x)$color <- colour_display(base.colour)$chosen
# 				V(x)$color[1] <- colour_display(base.colour=base.colour, start.colour)$start.node
# 		}else {
# 				y=NULL
# 			  }
# 	# Labelling
# 	E(x)$weight <- edge.v(edge.value)
# 	if(!no.label){
# 	V(x)$name = v.labels(names=names, newValue = newValue)$d.names
#      }else{
#      	V(x)$name= V(x)$name
#      	  }
# 	return(x)
# }



