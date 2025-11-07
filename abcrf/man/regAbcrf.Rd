\name{regAbcrf}
\alias{regAbcrf}
\alias{regAbcrf.formula}
\title{
Create a reg-ABC-RF object: a regression random forest from a reference table aimed out predicting posterior expectation, variance and quantiles for a parameter}
\description{
\code{regAbcrf} constructs a regression random forest from a reference table towards predicting posterior expectations, variances and quantiles of a parameter.
}
\usage{
\method{regAbcrf}{formula}(formula, data, ntree=500,
mtry=max(floor((dim(data)[2]-1)/3), 1), sampsize=min(1e5, nrow(data)),
paral=FALSE, ncores=if(paral) max(detectCores()-1,1) else 1, ...)
}
\arguments{
  \item{formula}{a formula: left of ~, variable representing
the response variable; right of ~, summary statistics of the reference table.}
  \item{data}{a data frame containing the reference table, composed of response variable (parameter) and summary statistics.}
  \item{ntree}{number of trees to grow in the forest, by default 500 trees.}
  \item{mtry}{Number of variables to possibly split at in each node. Default is the minimum between 1 and the number of variables divided by 3.}
  \item{sampsize}{size of the sample from the reference table used to grow a tree of the regression forest, by default the minimum between the number of elements of the reference table and 100,000.}
  \item{paral}{a boolean that indicates if the calculations of the regression random forest should be parallelized.}
  \item{ncores}{the number of CPU cores to use. If paral=TRUE, it is used the number of CPU cores minus 1. If ncores is not specified and \code{\link{detectCores}} does not detect the number of CPU cores with success then 1 core is used.}
  \item{\dots}{additional arguments to be passed on to \code{\link{ranger}} used to construct the regression random forest that predicts the response variable.}
}
\value{
An object of class \code{regAbcrf}, which is a list with the
following components:
\item{call}{the original call to \code{regAbcrf},}
\item{formula}{the formula used to construct the regression random forest,}
\item{model.rf}{an object of class \code{ranger} containing the trained forest with the reference table.}
}
\references{
Raynal L., Marin J.-M. Pudlo P., Ribatet M., Robert C. P. and Estoup, A. (2019)
\emph{ABC random forests for Bayesian parameter inference} Bioinformatics
\doi{10.1093/bioinformatics/bty867}
}

\seealso{
\code{\link{plot.regAbcrf}},
\code{\link{err.regAbcrf}},
\code{\link{predict.regAbcrf}},
\code{\link{covRegAbcrf}},
\code{\link{ranger}},
\code{\link{densityPlot}},
\code{\link{predictOOB}}.
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
model.rf.r
}
