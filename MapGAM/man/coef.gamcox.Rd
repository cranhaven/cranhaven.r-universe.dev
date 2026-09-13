\name{coef.gamcox}
\alias{coef.gamcox}
\title{
Coefficents of the \code{\link{gamcox}} Object
}
\description{
This function provides coefficients of a gamcox object produced by \code{\link{gamcox}}.
}
\usage{
\method{coef}{gamcox} (object,...) 
}
\arguments{
  \item{object}{
a \code{gamcox} object.
}
  \item{...}{.}
}
\author{
Lu Bai

Send bug reports to \email{sbartell@uci.edu}.
}
\seealso{
\code{\link{modgam}}
\code{\link{gamcox}}, 
\code{\link{predict.gamcox}}.
}
\examples{
\donttest{
data(CAdata)
fit <- gamcox(Surv(time,event)~lo(X,Y)+AGE+INS,data=CAdata, span = 0.2)
coef(fit)
}
}
\keyword{misc}
\keyword{smooth}
