\name{err.regAbcrf}
\alias{err.regAbcrf}
\title{Calculate and plot for different numbers of tree, the out-of-bag mean squared errors associated with a REG-ABC-RF object}
\description{
\code{err.regAbcrf} returns out-of-bag mean squared errors and plot them.
}
\usage{
err.regAbcrf(object, training, paral=FALSE, 
ncores= if(paral) max(detectCores()-1,1) else 1, what="mean")
}
\arguments{
  \item{object}{a \code{regAbcrf} object.}
  \item{training}{the data frame containing the reference table used to train the \code{\link{regAbcrf}} object.}
  \item{paral}{a boolean that indicates if random forests predictions should be parallelized.}
  \item{ncores}{the number of CPU cores to use for the random forest predictions. If paral=TRUE, it is used the number of CPU cores minus 1. If ncores is not specified and \code{\link{detectCores}} does not detect the number of CPU cores with success then 1 core is used.}
  \item{what}{a string caracter "mean" or "median" indicating if the predictions are computed with mean or median of the response variable.}
}
\value{
A matrix with 2 columns: the number of trees and the out-of-bag mean squared errors. NAs might be returned if the number of trees is too low. Errors are computed from 40 trees to the total number.
}
\references{
Raynal L., Marin J.-M. Pudlo P., Ribatet M., Robert C. P. and Estoup, A. (2019)
\emph{ABC random forests for Bayesian parameter inference} Bioinformatics
\doi{10.1093/bioinformatics/bty867}
}
\seealso{
  \code{\link{regAbcrf}},
  \code{\link{predict.regAbcrf}},
  \code{\link{plot.regAbcrf}},
  \code{\link{densityPlot}},
  \code{\link{covRegAbcrf}},
  \code{\link{ranger}}
}
\examples{
  data(snp)
  modindex <- snp$modindex
  sumsta <- snp$sumsta[modindex == "3",]
  r <- snp$param$r[modindex == "3"]
  r <- r[1:500]
  sumsta <- sumsta[1:500,]
  data2 <- data.frame(r, sumsta)
  model.rf.r <- regAbcrf(r~., data2, ntree=100)
  err.regAbcrf(model.rf.r, data2)
}