\name{GetInteractionStrength}
\alias{GetInteractionStrength}

\title{Strength of interactions}

\description{Computes the strength of second order interactions for covariates in a conditional random forest.}

\usage{
GetInteractionStrength(object, xnames=NULL)
}

\arguments{
  \item{object}{An object as returned by \code{\link[party]{cforest}} (or \code{\link{fastcforest}}).}
  \item{xnames}{character vector. The names of the variables for which to measure the strength of second order interactions. If NULL (default), all covariates are included.}
}

\note{
This function calls \code{vint} function from an old version of \code{vip} package for each interaction. The results are then gathered and reshaped into a friendly data frame format.
}

\value{
A data frame with pairs of variable names and the strength of the interaction between them.
}

\references{
Greenwell, B. M., Boehmke, B. C., and McCarthy, A. J.: A Simple and Effective Model-Based Variable Importance Measure. arXiv preprint arXiv:1805.04755 (2018).
}

\author{Nicolas Robette}

\seealso{
\code{\link{GetPartialData}},\code{\link{GetAleData}}
}

\examples{
  \dontrun{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2, 
              controls = party::cforest_unbiased(mtry=2, ntree=50))
  GetInteractionStrength(iris.cf)
  }
}

\keyword{tree}
