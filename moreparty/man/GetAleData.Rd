\name{GetAleData}
\alias{GetAleData}

\title{Accumulated Local Effects for a conditional random forest.}

\description{
Computes the Accumulated Local Effects for several covariates in a conditional random forest and gathers them into a single data frame.
}

\usage{
GetAleData(object, xnames=NULL, order=1, grid.size=20, parallel=FALSE)
}

\arguments{
\item{object}{An object as returned by \code{\link[party]{cforest}} (or \code{\link{fastcforest}}).}
\item{xnames}{A character vector of the covariates for which to compute the Accumulated Local Effects. If NULL (default), ALE are computed for all the covariates in the model. Should be of length 2 for 2nd order ALE.}
\item{order}{An integer indicating whether to compute 1st order ALE (1, default) or 2nd order ALE (2).}
\item{grid.size}{The size of the grid for evaluating the predictions. Default is 20.}
\item{parallel}{Logical indicating whether or not to run the function in parallel using a backend provided by the \code{foreach} package. Default is \code{FALSE}.}
}

\details{
The computation of Accumulated Local Effects uses \code{\link[iml]{FeatureEffect}} function from \code{iml} package for each covariate. The results are then gathered and reshaped into a friendly data frame format.
}

\value{
A data frame with covariates, their categories and their accumulated local effects.
}

\references{
Apley, D. W., Zhu J. "Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models". arXiv:1612.08468v2, 2019.

Molnar, Christoph. "Interpretable machine learning. A Guide for Making Black Box Models Explainable", 2019.
https://christophm.github.io/interpretable-ml-book/.}

\author{Nicolas Robette}

\seealso{
\code{\link[iml]{FeatureEffect}},\code{\link{GetPartialData}},\code{\link{GetInteractionStrength}}
}

\examples{
  \dontrun{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2, 
              controls = party::cforest_unbiased(mtry=2, ntree=50))
  GetAleData(iris.cf)
}}

\keyword{tree}