\name{print.summary.vht}
\alias{print.summary.vht}
\title{Print Method for Summary of Variance Homogeneity Test Results}

\usage{
\method{print}{summary.vht}(x, ...)}

\arguments{
  \item{x}{An object returned by \code{summary.vht()}.}
  \item{...}{Additional arguments.}
}
\description{
Prints the formatted summary of a \code{vht} object to the console.
}

\author{
Merve Kasikci and Osman Dag
}

\seealso{
\code{\link{summary.vht}}
}

\examples{
out <- bartletts.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
}

\keyword{functions}
