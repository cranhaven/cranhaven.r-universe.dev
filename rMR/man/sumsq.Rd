\name{sumsq}
\alias{sumsq}

\title{
Sum of squares
}
\description{
Internal Function for Use in Package for Calculating Sum of Squares of a Vector
}
\usage{
sumsq(x)
}

\arguments{
  \item{x}{
Numeric vector to be evaluated
}
}
\details{
Internal function for package
}
\value{
The sum of squares of the vector
}
\author{
Tyler L. Moulton
}
\examples{
vec <- sample(c(100:120), 50, replace = TRUE)
sumsq(vec)
}
