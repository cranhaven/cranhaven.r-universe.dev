\name{MatTrans.plot}
\alias{MatTrans.plot}
% Also NEED an '\alias' for EACH other topic documented here.
\title{ Mean coordinate plot}
\description{
Mean coordinate plot provided for the best fitted model returned by MatTrans.EM model.}
\usage{
MatTrans.plot(X, model = NULL, xlab = "", 
ylab = "", rownames = NULL, colnames = NULL, 
lwd.obs = 0.8, lwd.mean = 2, line.cols = NULL, ...)
}
% maybe also 'usage' for other objects documented here.
\arguments{
  \item{X }{ dataset of random matrices (p x T x n), n random matrices of dimensionality (p x T)}
  \item{model }{ fitted MatTrans mixture model returned by function MatTrans.plot()}
   \item{xlab }{ label on the X-axis}
  \item{ylab }{ label on the Y-axis}
   \item{rownames }{ input row variable names}
  \item{colnames }{ input column variable names}
   \item{lwd.obs }{ line width of observations}
  \item{lwd.mean }{ line width of the mean profile }
  \item{line.cols }{ line colors of the mean and observations }
   \item{...}{further arguments related to \code{\link{plot}} and \code{\link{lines}}}
}
\value{No return value, called for side effects}
\details{
Provides the mean profile plot for the fitted data by MatTrans.EM model. 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Mean profile plot }% __ONLY ONE__ keyword per line
