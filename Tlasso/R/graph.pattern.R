#' Graph Pattern Visualization
#'
#' Draw an undirected graph based on presicion matrix to present connection among variables.
#'
#' @param mat precision matrix that encodes information of graph struture.
#' @param main main title of graph. Default is \code{NULL}.
#' @param edge.color color of edge. Default is \code{"gray50"}.
#' @param vertex.color color of vertex. Default is \code{"red"}.
#' @param vertex.size size of vertex. Default is 3.
#' @param vertex.label label of vertex. Default is \code{NA}. 
#' @param thres thresholding level of substituting entry with zero, 
#' set entry to zero if its absolute value equals or is less than \code{thres}. 
#' If \code{thres} is negative or zero, no entry will be substituted with zero.
#' 
#' @details This function generates an udirected graph based on precision matrix. 
#' If an entry is zero, then no edge connects corresponding pair of nodes.
#' 
#' @return A plot of undirected graph.
#'
#' @author Xiang Lyu, Will Wei Sun, Zhaoran Wang, Han Liu, Jian Yang, Guang Cheng. 
#' @seealso \code{\link{infer.analysis}}, \code{\link{est.analysis}}
#'
#' @examples
#'  
#' graph.pattern(ChainOmega(5, sd = 13))
#' # a triangle graph
#' 
#' @export
#'
#' @import igraph
#' @importFrom graphics plot
#' 
#' 

graph.pattern = function(mat, main=NULL, edge.color = "gray50", vertex.color = "red", vertex.size = 3, vertex.label = NA, thres=1e-5 ){
  
  if (!is.matrix(mat)) {
    stop('argument mat should be a matrix')
  } else if (dim(mat)[1]!=dim(mat)[2]) {
    stop('argument mat should be symmetric')
  }  
  
  connect.mat = mat
  p = ncol(mat)
  for(i in 1:p){
    for(j in 1: p){
      connect.mat[i,j] = ifelse(abs(mat[i,j])>thres, 1, 0)
    }
  }
  
  g = graph.adjacency(connect.mat, mode = "undirected", diag = FALSE)
  layout.grid = layout.fruchterman.reingold(g)

  plot(g, layout = layout.grid, edge.color = edge.color, vertex.color = vertex.color, vertex.size = vertex.size, vertex.label = vertex.label, main = main)

} 
