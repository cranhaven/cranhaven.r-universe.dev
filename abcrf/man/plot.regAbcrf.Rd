\name{plot.regAbcrf}
\alias{plot.regAbcrf}
\title{Plot of a reg-ABC-RF object}
\description{
\code{plot.regAbcrf} provides a variable importance plot used to construct the reg-ABC-RF object, as measured by \code{\link{ranger}} with the argument importance='impurity'.
}
\usage{
\method{plot}{regAbcrf}(x, n.var=min(30, length(x$model.rf$variable.importance)), xlim=NULL, main=NULL, ...)
}
\arguments{
\item{x}{a \code{\link{regAbcrf}} object.}
\item{n.var}{number of variables in the variable importance representation. The default value is equal to the minimum between 30 and the number of summary statistics.}
\item{xlim}{range of the abscissa for the variable importance plot.}
\item{main}{an overall title for the variable importance plot.}
\item{...}{not used.}
}
\references{
Raynal L., Marin J.-M. Pudlo P., Ribatet M., Robert C. P. and Estoup, A. (2019)
\emph{ABC random forests for Bayesian parameter inference} Bioinformatics
\doi{bioinformatics/bty867}
}
\seealso{
\code{\link{regAbcrf}},
\code{\link{predict.regAbcrf}},
\code{\link{err.regAbcrf}},
\code{\link{covRegAbcrf}},
\code{\link{ranger}},
\code{\link{densityPlot}}
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
plot(model.rf.r)
}
