\name{formula.gamcox}
\alias{formula.gamcox}
\title{
Formula of the \code{\link{gamcox}} Object
}
\description{
This function provides the formula of a gamcox object produced by \code{\link{gamcox}}.
}
\usage{
\method{formula}{gamcox} (x,...) 
}
\arguments{
  \item{x}{
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
formula(fit)
}
}
\keyword{misc}
\keyword{smooth}
