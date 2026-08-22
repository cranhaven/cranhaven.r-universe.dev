\name{summary.vht}
\alias{summary.vht}
\title{Summary Method for Variance Homogeneity Test Results}


\usage{
\method{summary}{vht}(object, ...)}

\arguments{
  \item{object}{An object of class \code{vht}, typically returned by variance homogeneity tests.}
  \item{...}{Additional arguments.}
}

\value{
  Prints a summary to the console.
}

\description{
Provides a concise summary of the results from a variance homogeneity test in the package.
}

\details{
This method is specifically designed for objects of class \code{vht}. It prints test method, dependent variable, grouping variable, test statistic, degrees of freedom, and p-value.
}

\author{
Merve Kasikci and Osman Dag
}

\examples{
out <- bartletts.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
}

\keyword{functions}
