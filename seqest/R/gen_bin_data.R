#' @title generate the data used for the model experiment
#'
#' @description
#' \code{gen_bin_data} generate the data used for the model experiment
#'
#' @details
#' The function gen_bin_data generates N points. That is,the first column of the
#' design matrix is 1 and the second column has a normal distribution with a
#' mean of 1 and a variance of 1 and the rest columns with a mean of 0 and a
#' variance of 1. Next, they are clustered into classes to decrease the
#' computation cost. You should specify the number of classes. In the function,
#' it's the parameter nclass.
#' @param beta A numeric vector that represents the true coefficients that used to
#'   generate the synthesized data.
#' @param N A numeric number specifying the number of the synthesized data. It
#'   should be an integer.
#' @param nclass A numeric number used to specify how many clusters the original
#'   data would be transformed into. It should be an integer.
#' @param seed Set random number seed.
#' @export
#' @return a list of seven elements:
#' \item{data.clust}{list with clustering results. Samples in the same list
#' element are closer with each other}
#' \item{X}{the samples with the smallest variance from each cluster. Note that
#' the length of X is the same as the number of data.clust}
#' \item{y}{the target value of 0 or 1 corresponding to X}
#'
#'
#' @references {
#' Wang Z, Kwon Y, Chang YcI (2019). Active learning for binary classification
#' with variable selection. arXiv preprint arXiv:1901.10079.
#' }
#'
#' @seealso{
#'    \code{\link{gen_multi_data}} for categorical and ordinal case
#'
#'    \code{\link{gen_GEE_data}} for generalized estimating equations case.
#'}
#'
#' @examples
#' # For an example, see example(seq_bin_model)

gen_bin_data <- function(beta, N, nclass, seed){
  set.seed(seed)
  pnum <- length(beta)
  X <- matrix(stats::rnorm((pnum - 1)*N),N)
  X[,1] <- X[,1]+1
  Xfix <- cbind(1,X)
  z <- c(Xfix%*%beta)
  ptmp <- exp(z)/(1+exp(z))
  Yfix <- stats::rbinom(N,1,ptmp)
  tmp <- stats::kmeans(Xfix,nclass)
  cluster <- tmp$cluster
  center <- tmp$center

  xclust <- NULL
  yclust <- NULL
  data.clust <- vector('list',nclass)

  for(i in 1:nclass)
  {
    lab <- which(cluster==i)
    xs <- Xfix[lab,,drop=FALSE]
    ys <- Yfix[lab]

    ceni <- center[i,,drop=FALSE]
    tmp <- apply(xs,1,function(x){mean((x-ceni)^2)})
    loc <- which(tmp==min(tmp))

    xclust <- rbind(xclust,xs[loc[1],])
    yclust <- c(yclust,ys[loc[1]])
    data.clust[[i]] <- cbind(ys,xs)
  }
  return(list(data.clust = data.clust, X = xclust, y = yclust))
}
