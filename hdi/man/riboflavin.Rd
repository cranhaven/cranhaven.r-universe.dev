\name{riboflavin}
\alias{riboflavin}
\docType{data}
\title{Riboflavin data set}
\description{Dataset of riboflavin production by Bacillus subtilis
  containing \eqn{n=71} observations of \eqn{p=4088} predictors (gene
  expressions) and a one-dimensional response (riboflavin production).}
\usage{data(riboflavin)}
\format{
  \describe{
    \item{y}{Log-transformed riboflavin production rate (original name:
      q_RIBFLV).}
    \item{x}{(Co-)variables measuring the logarithm of the expression
      level of 4088 genes.}
  }
}
\details{Data kindly provided by DSM (Switzerland).}

\references{
  \enc{Bühlmann}{Buhlmann}, P., Kalisch, M. and Meier,
  L. (2014) \emph{High-dimensional statistics with a view towards
  applications in biology}. Annual Review of Statistics and
  its Applications \bold{1}, 255--278
}

\examples{data(riboflavin)}
\keyword{datasets}
