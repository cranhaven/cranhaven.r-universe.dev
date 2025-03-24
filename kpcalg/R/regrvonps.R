#' Check if variable can be regressed to independence on its parents
#'
#' Uses the generalised additive model \link{gam} to non-linearly and non-parametrically regress variable V on its parents and set of variables S.
#'
#' @param G adjacency matrix, for the graph
#' @param V integer, node which we regress
#' @param S integer(s), set we regress on
#' @param suffStat sufficient statistics to perform the independence test \link{kernelCItest}
#' @param indepTest independence test to check for dependence between residuals of V and S
#' @param alpha numeric cutoff for significance level of individual partial correlation tests
#' @import methods
#' @export
#' @return regrVonPS() returns the number of p-values smaller than the cutoff, i.e 0 means residuals of V are independent of all variables in S
#' @author Petras Verbyla (\email{petras.verbyla@mrc-bsu.cam.ac.uk})
#'

regrVonPS <- function(         G,           # adj matrix
                               V,           # the node I'm investigating
                               S,           # some ( not always all ) nodes to which v is undirected connected
                               suffStat,    # sufficient statistics
                               indepTest=kernelCItest,   # independence test to be used to check independence
                               alpha=0.2       # test level
                               ){

  parentsV <- which(G[,V]==1 & G[V,]==0, arr.ind = T)
  pvalvec <- matrix(0,ncol=length(S))

  test.data <- suffStat$data[,c(V,S)]
  residV <- regrXonS(suffStat$data[,V],suffStat$data[,c(S,parentsV)])

  test.suffStat <- suffStat
  test.suffStat$data <- cbind(residV,suffStat$data[,S])

  for (i in 1:length(S)){
    pvalvec[i] <- indepTest(x=1, y=(i+1), suffStat = test.suffStat)
  }
  output <- sum(pvalvec<alpha)

}
