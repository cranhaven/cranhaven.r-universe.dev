\name{ggVarImp}
\alias{ggVarImp}
\title{Dot plot of variable importance}

\description{
Plots the importance of the covariates of a supervised learning model in a dot plot.
}

\usage{
ggVarImp(importance, sort=TRUE, xlabel="Importance", ylabel="Variable", main="")
}

\arguments{
  \item{importance}{numeric vector. The vector of the importances of the covariates. Should be a named vector.}
  \item{sort}{logical. Whether the vector of importances should be sorted or not. Default is TRUE.}
  \item{xlabel}{character. Title of the x axis.}
  \item{ylabel}{character. Title of the y axis.}
  \item{main}{character. Title of the plot.}
}

\author{Nicolas Robette}

\seealso{
\code{\link[varImp]{varImp}},\code{\link[varImp]{varImpAUC}},\code{\link{fastvarImp}},\code{\link{fastvarImpAUC}}
}

\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2,
            control = party::cforest_unbiased(mtry = 2, ntree = 50))
  imp <- fastvarImpAUC(object = iris.cf, parallel = FALSE)
  ggVarImp(imp)
}

\keyword{ aplot }
\keyword{ tree }
