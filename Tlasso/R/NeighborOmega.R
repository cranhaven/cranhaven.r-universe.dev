#' Precision Matrix of Nearest-Neighbor Graph
#'
#' Generate precision matrix of nearest-neighbor network following the set-up in \href{https://paperity.org/p/38773767/gradient-directed-regularization-for-sparse-gaussian-concentration-graphs-with}{Li and Gui (2006)}
#' and \href{https://www.jmlr.org/papers/volume16/lee15a/lee15a.pdf}{Lee and Liu (2006)}.
#'
#' @param p dimension of generated precision matrix.
#' @param sd seed for random number generation. Default is 1.
#' @param knn sparsity of precision matrix, i.e., matrix is generated from a \code{knn} nearest-neighbor graph. \code{knn} should be less than \code{p}. Default is 4.
#' @param norm.type normalization methods of generated precision matrix, i.e., \eqn{\Omega_{11} = 1}{\Omega_{11}=1} 
#' if norm.type = 1 and \eqn{\|\Omega\|_{F}=1}{||\Omega||_F =1 } if norm.type = 2. Default value is 2. 
#'
#' @details For a \code{knn} nearest-neighbor graph, this function first randomly picks p points from a 
#' unit square and computes all pairwise distances among the points. Then it searches for the knn nearest-neighbors
#' of each point and a pair of symmetric entries in the precision matrix that has a random chosen value from \eqn{[-1, -0.5] \cup [0.5, 1]}{[-1, -0.5] U [0.5, 1]}. Finally, to 
#' ensure positive definite property, it normalizes the matrix as \eqn{\Omega <- \Omega + (\lambda (\Omega) + 0.2 ) 1_p}{\Omega <- \Omega + (\lambda (\Omega)+0.2) 1_p} where 
#' \eqn{\lambda (\cdot )}{\lambda (.)} refers to the samllest eigenvalue.
#' 
#' 
#' @return A precision matrix generated from the \code{knn} nearest-neighor graph.
#'
#' @author Xiang Lyu, Will Wei Sun, Zhaoran Wang, Han Liu, Jian Yang, Guang Cheng. 
#' @seealso \code{\link{ChainOmega}}
#'
#' @examples
#' 
#' m.vec = c(5,5,5)  # dimensionality of a tensor 
#' n = 5   # sample size 
#' knn=4 # sparsity 
#' 
#' Omega.true.list = list()
#' 
#' for ( k in 1:length(m.vec)){
#'   Omega.true.list[[k]] = NeighborOmega(m.vec[k],knn=4, sd=k*100,norm.type=2)
#' }
#' Omega.true.list  # a list of length 3 contains precision matrices from 4-nearnest neighbor graph
#' 
#' @export
#'
#' @importFrom stats runif rbinom
#'


NeighborOmega = function(p, sd = 1,knn = 4, norm.type = 2){

  if (!(norm.type==1 | norm.type==2)){
    stop('Please input a correct argument norm.type')
  } else if ( !((p==round(p))&((p>1)|(p==1) )) ){
    stop('argument p should be a positive integer')
  } else if ( !((knn==round(knn))&((knn>1)|(knn==1) )) ){
    stop('argument knn should be a positive integer')
  } else if ( {p<knn} | {p==knn} ) {
    stop('argument p should be greater than knn')
  }
  
  set.seed(sd)
  x = runif(p, 0, 1)
  y = runif(p, 0, 1)

  d = matrix(NA, p, p)
  Omega = matrix(0, p, p)

  for(i in 1:p){
    for(j in 1:p){
      d[i,j] = sqrt((x[i]-x[j])^2 + (y[i]-y[j])^2)
    }
  }

  m.set = matrix(NA,p,knn)
  for(i in 1:p){
    m.set[i,] = order(d[i,])[2:(knn+1)]
  }

  for(i in 1:p){
    id = m.set[i,]
    for(j in 1:knn){
      while( {sum(m.set[id[j],] == i) ==1} & {Omega[i,id[j]] == 0 }){
        Omega[i,id[j]] = runif(1, 0.5, 1)*(-1)^rbinom(1,1,0.5)
        Omega[id[j], i] = Omega[i, id[j]]
      }
    }

  }

  diag(Omega) = 1

  Omega = Omega + (abs(min(eigen(Omega)$values)) + 0.2 )* diag(p)

  if(norm.type == 2){
    Omega = Omega/norm(Omega,type="F")
  }else if(norm.type == 1) {
    Omega = Omega/Omega[1,1]
  }

  return(Omega)
}
