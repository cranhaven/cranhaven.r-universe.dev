\name{mph.summary}
\alias{mph.summary}
\title{
Summary Statistics of the Fitted MPH Model
}
\description{
Computes and prints a collection of summary statistics of the
fitted MPH model.

This function is used in conjunction with the ML fitting function
\code{mph.fit}.
}
\usage{
mph.summary(mph.out, cell.stats = FALSE, model.info = FALSE, digits = 4)
}
\arguments{
  \item{mph.out}{Result of \code{mph.fit}.}
  \item{cell.stats}{Logical variable indicating whether cell specific
                    statistics are to be output. Default:
                    \code{cell.stats = FALSE}.}
  \item{model.info}{Logical variable indicating whether model
                    information is to be output. Default:
                    \code{model.info = FALSE}.}
  \item{digits}{Integer giving output precision; used in the
                \code{round()} function.}
}
\value{
\code{NULL}
}
\references{
Lang, J. B. (2004) Multinomial-Poisson homogeneous models for contingency tables, \emph{Annals of Statistics}, \bold{32}, 340--383.

Lang, J. B. (2005) Homogeneous linear predictor models for contingency tables, \emph{Journal of the American Statistical Association}, \bold{100}, 121--134.
}
\author{
Joseph B. Lang
}
\seealso{
\code{\link{mph.fit}}
}
\keyword{htest}% use one of  RShowDoc("KEYWORDS")
