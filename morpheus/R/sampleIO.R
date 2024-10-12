#' Generate sample inputs-outputs
#'
#' Generate input matrix X of size nxd and binary output of size n, where Y is subdivided
#' into K groups of proportions p. Inside one group, the probability law P(Y=1) is
#' described by the corresponding column parameter in the matrix beta + intercept b.
#'
#' @name generateSampleIO
#'
#' @param n Number of individuals
#' @param p Vector of K(-1) populations relative proportions (sum (<)= 1)
#' @param beta Vectors of model parameters for each population, of size dxK
#' @param b Vector of intercept values (use rep(0,K) for no intercept)
#' @param link Link type; "logit" or "probit"
#'
#' @return A list with
#' \itemize{
#'   \item{X: the input matrix (size nxd)}
#'   \item{Y: the output vector (size n)}
#'   \item{index: the population index (in 1:K) for each row in X}
#' }
#'
#' @examples
#' # K = 3 so we give first two components of p: 0.3 and 0.3 (p[3] = 0.4)
#' io <- generateSampleIO(1000, c(.3,.3),
#'   matrix(c(1,3,-1,1,2,1),ncol=3), c(.5,-1,0), "logit")
#' io$index[1] #number of the group of X[1,] and Y[1] (in 1...K)
#'
#' @export
generateSampleIO <- function(n, p, beta, b, link)
{
  # Check arguments
  tryCatch({n <- as.integer(n)}, error=function(e) stop("Cannot convert n to integer"))
  if (length(n) > 1)
    warning("n is a vector but should be scalar: only first element used")
  if (n <= 0)
    stop("n: positive integer")
  if (!is.matrix(beta) || !is.numeric(beta) || any(is.na(beta)))
    stop("beta: real matrix, no NAs")
  K <- ncol(beta)
  if (!is.numeric(p) || length(p)<K-1 || any(is.na(p)) || any(p<0) || sum(p) > 1)
    stop("p: positive vector of size >= K-1, no NA, sum(<)=1")
  if (length(p) == K-1)
    p <- c(p, 1-sum(p))
  if (!is.numeric(b) || length(b)!=K || any(is.na(b)))
    stop("b: real vector of size K, no NA")

  # Random generation of the size of each population in X~Y (unordered)
  classes <- rmultinom(1, n, p)

  d <- nrow(beta)
  zero_mean <- rep(0,d)
  id_sigma <- diag(rep(1,d))
  X <- matrix(nrow=0, ncol=d)
  Y <- c()
  index <- c()
  for (i in 1:K)
  {
    index <- c(index, rep(i, classes[i]))
    newXblock <- MASS::mvrnorm(classes[i], zero_mean, id_sigma)
    arg_link <- newXblock %*% beta[,i] + b[i]
    probas <-
      if (link == "logit")
      {
        e_arg_link = exp(arg_link)
        e_arg_link / (1 + e_arg_link)
      }
      else #"probit"
        pnorm(arg_link)
    probas[is.nan(probas)] = 1 #overflow of exp(x)
    X <- rbind(X, newXblock)
    Y <- c( Y, vapply(probas, function(p) (rbinom(1,1,p)), 1) )
  }
  shuffle <- sample(n)
  list("X"=X[shuffle,], "Y"=Y[shuffle], "index"=index[shuffle])
}
