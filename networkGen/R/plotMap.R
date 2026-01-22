#' Creating the the plot for the logic map.#'
#'#' @author Aiden Loe
#'#' @export
#'#' @importFrom graphics plot
#'#' @title plot.logic.map
#'#' @description To make it easier to plot the map in a png format.
#'#' @details Most of the internal functions are based on the igraph packge. This acts as a wrapper to the functions to make it easier for people to redesign the plots. However, the changes made here do not truly affect the way it is generated in html.
#'#' @param x This is the logic of the object we are goig to plot which is based on \code{closedMaps}.
#'#' @param seed This will always result in the same graph
#'#' @param v.size Is the size of the vertice
#'#' @param vertex.shape This determines the shape of the vertex shape
#'#' @param vertex.label.cex This is the size of the vertex label
#'#' @param layout Layout of the graph which graph
#'#' @author Aiden Loe
#'#' @examples
#' x <-  nodeLogic(2,"trail", 2)
#' plot.logic.map(x,seed = NULL, v.size = 15, layout =layout_with_dh)

# plot.logic.map <- function(x,seed=NULL, v.size=10,vertex.shape="square",vertex.label.cex=0.5 ,layout=layout_with_dh){
#
# 	if(!is.null(seed))
# 		set.seed(seed)
# 		plot(x,
# 		layout=layout,
# 		vertex.shape=vertex.shape,
# 		vertex.size=v.size,
# 		vertex.label.cex=vertex.label.cex)
#
# }





## Multi Map (10 times)
#' #' @export
#' #' @title plot.multi.map
#' #' #' @param png Tells you to print to your working directory automatically or not.
#' #' @param x is the object created by \code{closedMaps} or \code{g.maps}
#' #' @param n is the number of times you want to generate the plot
#' #' @param v.size Is the size of the vertice
#' #' @param height This is the height of the image
#' #' @param width This is the width of the image
#' #' @param seed This will always result in the same graph
#' #' @param png_name This is the input of the name
#' #' @param vertex.shape This determines the shape of the vertex shape
#' #' @param vertex.label.cex This is the size of the vertex label
#' #' @param layout Layout of the graph which graph
#' #' @param xlab The label of the x coordinates
#' #' @param ylab The label of the y coordinates
#' #' @param cex.lab The size of the labels
#'
#'
#' plot.multi.map <- function(x, n, v.size, height, width, png=TRUE, layout, vertex.label.cex, vertex.shape, xlab, ylab, cex.lab){
#' 	for(i in 1:n){
#' 	png_name <- paste0("map", i, ".png")
#' 	plot.logic.map(x, png=png, v.size=v.size, height=height, width=width, seed=100 + i , png_name=png_name ,vertex.shape=vertex.shape, vertex.label.cex=vertex.label.cex, layout=layout, xlab=xlab, ylab=ylab, cex.lab=cex.lab)
#' 	}
#' }




