#' The function of converting the adjacency matrix into the topological layer.
#'
#' @author Mingyang Ren <renmingyang17@mails.ucas.ac.cn>.
#' @references Zhao, R., He X., and Wang J. (2022). Learning linear non-Gaussian directed acyclic graph with diverging number of nodes. Journal of Machine Learning Research.
#' @usage layer_adj(true_adjace)
#'
#' @description The function of converting the adjacency matrix into the topological layer.
#' @param true_adjace a p * p adjacency matrix
#' 
#' @return Layer_true: a p * 2 matrix to store the information of layer. The first column is the node label, and the second column is the corresponding layer labels.
#' 
#' @export
#'
#'
#'

layer_adj = function(true_adjace){
  p = dim(true_adjace)[2]
  Layer_true=matrix(c(1:p,rep(NA,p)),p,2)
  S = 1:p
  true_adjace_S = true_adjace
  t=0
  while (length(S)>0) {
    true_adjace_S = as.matrix(true_adjace[S,S])
    low_nodes = S[apply(true_adjace_S, 1, function(x) sum(x^2)==0)]
    Layer_true[low_nodes,2] = t
    S = setdiff(S, low_nodes)
    t = t+1
  }
  return(Layer_true)
}

