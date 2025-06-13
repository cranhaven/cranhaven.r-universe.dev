#' For given scores \code{Y} and dynamic principal components \code{XI}
#' retrive a series from which scores \code{Y} were calculated.
#' This procedure should be seen as the inverse of \code{\link{pcdpca.scores}}.
#'
#' @title Retrieve a process from given scores
#' @param Y scores process
#' @param XI principal components series
#' @return Retrived process X
#' @seealso \code{\link{pcdpca.scores}}, \code{\link{pcdpca}}
#' @references Kidzinski, Kokoszka, Jouzdani
#' Dynamic principal components of periodically correlated functional time series
#' Research report, 2016
#' @export
pcdpca.inverse = function(Y,XI){
  n = nrow(Y)
  d = ncol(Y)
  period = XI$period

  Y.st = pc2stat(Y, period = period)

  # reorder columns
  idx = c()
  for (i in (d-1):0)
    idx = c(idx,(1:period * d - i))
  Y.st = Y.st[,idx]

  X = freqdom::filter.process(Y.st,t(rev(XI)))
  matrix(t(X),ncol=d,byrow = T)
}
