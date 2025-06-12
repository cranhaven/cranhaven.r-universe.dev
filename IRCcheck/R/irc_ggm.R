#' Irrepresentable Condition: Gaussian Graphical Model
#' 
#' @description Check the IRC (or  Incoherence condition) in Gaussian graphical Models, 
#'              following Equation (8) in \insertCite{ravikumar2008model}{IRCcheck}.
#'              
#' @param true_network A matrix of dimensions \emph{p} by \emph{p}, assumed to be 
#'                     a partial correlation matrix.
#'                     
#' @param cores Integer. Number of cores for parallel computing (defaults to \code{2})
#' 
#' @references
#' \insertAllCited{}
#'
#' @return infinity norm (greater than 1 the IRC is violated, with closer to zero better).
#' @export
#' 
#' @importFrom parallel makeCluster stopCluster detectCores parSapply
#' @importFrom corpcor pcor2cor
#' @importFrom stats cov2cor
#' 
#' @examples 
#' \donttest{
#' # generate network
#' net <- gen_net(p = 20, edge_prob = 0.3, lb = 0.05, ub = 0.3)
#' 
#' # check irc
#' irc_ggm(net$pcors)
#' 
#' # random adj 
#' # 90 % sparsity (roughly)
#' p <- 20
#' adj <- matrix(sample(0:1, size = p^2, replace = TRUE, 
#'               prob = c(0.9, 0.1) ), 
#'               nrow = p, ncol = p)
#' 
#' adj <- symm_mat(adj)
#' 
#' diag(adj) <- 1
#' 
#' # random correlation matrix
#' set.seed(1)
#' cors <- cov2cor(
#'   solve(
#'   rWishart(1, p + 2, diag(p))[,,1])
#' )
#' 
#' # constrain to zero
#' net <- constrained(cors, adj = adj)
#' 
#' irc_ggm(net$wadj)
#' 
#' 
#' #' # random adj 
#' # 50 % sparsity (roughly)
#' p <- 20
#' adj <- matrix(sample(0:1, size = p^2, replace = TRUE, prob = c(0.5, 0.5) ), 
#'               nrow = p, ncol = p)
#' 
#' adj <- symm_mat(adj)
#' diag(adj) <- 1
#' 
#' # random correlation matrix
#' set.seed(1)
#' cors <- cov2cor(
#'   solve(
#'   rWishart(1, p + 2, diag(p))[,,1])
#' )
#' 
#' # constrain to zero
#' net <- constrained(cors, adj = adj)
#' 
#' irc_ggm(net$wadj)
#' 
#' }
irc_ggm <- function(true_network, cores = 2){
  
  diag(true_network) <- 1
  
  cl <- parallel::makeCluster(cores)
  
  cors <- corpcor::pcor2cor(true_network)
  
  adj <- ifelse(true_network == 0, 0, 1)
  
  kron <- kronecker(cors, cors)
  
  scs <- Gamma_ScS(adj, kron, cl)
  
  ss <- Gamma_SS(adj, kron, cl)
  
  parallel::stopCluster(cl)
  
  infinity_norm <- norm(scs %*% solve(ss), type = "i")
  
  return(infinity_norm)
}