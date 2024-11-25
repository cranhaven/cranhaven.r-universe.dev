#' Semidefinite Quadratic Linear Programming Solver
#' 
#' \code{sqlp} solves a semidefinite quadratic linear programming problem using the SDPT3 algorithm of Toh et. al. (1999)
#' returning both the primal solution X and dual solution Z.
#' 
#' @details A full mathematical description of the problem to be solved, details surrounding the
#' input variables, and discussion regarding the output variables can be found in the accompanying vignette.
#' 
#' @param blk A named-list object describing the block diagonal structure of the SQLP data
#' @param At A list object containing constraint matrices for the primal-dual problem
#' @param C A list object containing the constant $c$ matrices in the primal objective function
#' @param b A vector containing the right hand side of the equality constraints in the primal problem
#' @param control A list object specifying the values of certain parameters. If not provided, default values are used
#' @param X0 An initial iterate for the primal solution variable X. If not provided, an initial iterate is computed internally.
#' @param y0 An initial iterate for the dual solution variable y. If not provided, an initial iterate is computed internally.
#' @param Z0 An initial iterate for the dual solution variable Z. If not provided, an initial iterate is computed internally.
#' 
#' @return 
#' \item{X}{A list containing the solution matrix to the primal problem}
#' \item{y}{The  solution vector to the dual problem}
#' \item{Z}{A list containing the  solution matrix to the dual problem}
#' \item{pobj}{The achieved value of the primary objective function}
#' \item{dobj}{The achieved value of the dual objective function}
#' 
#' @examples
#' 
#' blk = c("l" = 2)
#' C = matrix(c(1,1),nrow=1)
#' A = matrix(c(1,3,4,-1), nrow=2)
#' At = t(A)
#' b = c(12,10)
#' out = sqlp(blk,list(At),list(C),b)
#' 
#' @references 
#' K.C. Toh, M.J. Todd, and R.H. Tutuncu, SDPT3 --- a Matlab software package for semidefinite programming, Optimization Methods and Software, 11 (1999), pp. 545--581.
#' R.H Tutuncu, K.C. Toh, and M.J. Todd, Solving semidefinite-quadratic-linear programs using SDPT3, Mathematical Programming Ser. B, 95 (2003), pp. 189--217.
#' 
#' @export
#' @import Matrix
#' @importFrom methods is
#' @importFrom stats rnorm runif
#' @useDynLib sdpt3r, .registration = TRUE
sqlp <- function(blk = NULL, At = NULL, C = NULL, b = NULL,
                          control = NULL, X0 = NULL, y0 = NULL, Z0 = NULL) {
  
  blk_tmp = matrix(list(),nrow=length(blk),ncol=2)
  
  for(i in 1:length(blk)){
    blk_tmp[[i,1]] = names(blk)[i]
    blk_tmp[[i,2]] = blk[i]
  }
  
  out <- sqlp_base(blk = blk_tmp,
              At = as.matrix(At),
              C = as.matrix(lapply(C, as.matrix)),
              b = as.matrix(b),
              OPTIONS = control, X0 = X0, y0 = y0, Z0 = Z0)
  dim(out$X) <- NULL
  dim(out$Z) <- NULL
  return(out)
}