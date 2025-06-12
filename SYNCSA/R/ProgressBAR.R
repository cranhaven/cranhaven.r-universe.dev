#' @title Text Progress Bar
#'
#' @description Text progress bar in the R console. See \code{\link{txtProgressBar}}.
#'
#' @encoding UTF-8
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @param n Number of the current progress.
#' @param N Total number of cases.
#' @param ... Other parameters for the txtProgressBar function.
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @keywords SYNCSA
#' @export
ProgressBAR<-function(n, N, ...)
{
  n <- n/N
  for(i in c(0, n)){
    A <- utils::txtProgressBar(...)
    utils::setTxtProgressBar(A, i)
  }
}
