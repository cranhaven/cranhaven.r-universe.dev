#' Preprocess
#'
#'@description Performs the pre-processing of a data matrix such that it is ready
#'to be used by \code{vbmfa}.
#'
#' @param Y An \emph{n} by \emph{p} data matrix which is to be scaled.
#' @param ppp An optional \emph{p} by 2 matrix where the columns represent
#' the sample mean and sample standard deviation of the \emph{p}th dimension of \code{Y}.
#' @param shrinkQ If 1, the data is shrunk according to \code{ppp}. If 0, the data is expanded
#' to invert a prior shrinking by \code{ppp}.
#'
#' @return A list containing \itemize{
#' \item{\code{Yout}:}{ A processed data matrix of observations.}
#' \item{\code{ppp}:}{ The shrinkage which as applied in the processing.}
#' }
#' @export
#'
#' @references \insertRef{ghahramani2000variational}{autoMFA}
#' @examples
#' Yout <- preprocess(autoMFA::MFA_testdata);
#'
#'@seealso \code{\link{vbmfa}} for fitting models after using \code{preprocess}.
preprocess <- function(Y,ppp,shrinkQ){

  #Matthew J. Beal GCNU 10/10/00

  Y <- t(Y)
  n = dim(Y)[2];

  if (missing(ppp)){
    if(!missing(shrinkQ)){warning("As ppp was not specified, shinkQ is ignored and normal preprocessing is assumed.")}
    m = matrix(rowMeans(Y),ncol = 1) #mean
    std = matrix(sqrt(rowSums((Y - matrix(rep(m,n),ncol=n))^2/(n))),ncol=1) #sd normalised by n
    ppp = cbind(m,std)
    Y  = Y - matrix(rep(m,n),ncol=n)
    Y = diag(as.vector(1/std))%*%Y;
  } else {

    if(missing(shrinkQ)){
      warning("As ppp was specified but shinkQ was not, inverting shrinkage is assummed.")
      shrinkQ = 0
    }

    # ppp is specified, but how to use it?
    if (shrinkQ == 0) {
      # want to INVERT a shrinking
      Y = diag(as.vector(ppp[,2]))%*%Y;
      Y = Y + matrix(rep(ppp[,1],n),ncol = n)
    } else {
      # want to do a SIMILAR shrinking (on test data perhaps)
      Y = Y - matrix(rep(ppp[,1],n),ncol = n);
      Y = diag(as.vector(1./ppp[,2]))%*%Y;
    } }
  return(list(Yout = t(Y), ppp = ppp))
}
