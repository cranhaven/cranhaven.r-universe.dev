\name{SurrogateTree}
\alias{SurrogateTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Surrogate tree for conditional inference random forests}
\description{
Builds a surrogate tree to approximate a conditional random forest model.}
\usage{
SurrogateTree(object, mincriterion = 0.95, maxdepth = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object as returned by \code{cforest} (or \code{fastcforest}).}

  \item{mincriterion}{the value of the test statistic (for \code{testtype == "Teststatistic"}),
                       or 1 - p-value (for other values of \code{testtype}) that
                       must be exceeded in order to implement a split.}

  \item{maxdepth}{maximum depth of the tree. Default is 3.}
}

\details{
A global surrogate model is an interpretable model that is trained to approximate the predictions of a black box model (see Molnar 2019). Here a conditional inference tree is build to approximate the prediction of a conditional inference random forest. Practically, the surrogate tree takes the forest predictions as response and the same predictors as the forest.
}

\note{
The surrogate tree is built using \code{ctree} from \code{partykit} package.
}

\value{
A list withe following items :
\item{tree}{The surrogate tree, of class \code{party}}
\item{r.squared}{The R squared of a linear regression with random forests prediction as dependent variable and surrogate tree prediction as predictor}
}

\references{
Molnar, Christoph. "Interpretable machine learning. A Guide for Making Black Box Models Explainable", 2019.
https://christophm.github.io/interpretable-ml-book/.}

\author{
Nicolas Robette
}

\seealso{
\code{cforest}, \code{ctree}
}

\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2,
            control = party::cforest_unbiased(mtry = 2, ntree = 50))
  surro <- SurrogateTree(iris.cf)
  surro$r.squared
  plot(surro$tree)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
