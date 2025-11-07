\name{variableImpPlot}
\alias{variableImpPlot}
\title{Variable importance plot from a random forest}
\usage{
variableImpPlot(object, 
n.var=min(30, length(object$model.rf$variable.importance)),
xlim=NULL, main=NULL)
}
\description{
\code{variableImpPlot} provides a dotchart of variable importance as measured by \code{\link{ranger}} with the argument importance='impurity'.
}
\arguments{
\item{object}{an \code{abcrf} or \code{regAbcrf} object.}
\item{n.var}{number of variables in the variable importance representation.}
\item{xlim}{range of the abscissa.}
\item{main}{an overall title for the variable importance plot.}
}
\value{
Invisibly, the importance of the variables that were plotted.
}

\seealso{
\code{\link{abcrf}},
\code{\link{plot.abcrf}},
\code{\link{plot.regAbcrf}}
}

\examples{
data(snp)
modindex <- snp$modindex[1:500]
sumsta <- snp$sumsta[1:500,]
data1 <- data.frame(modindex, sumsta)
model.rf <- abcrf(modindex~., data1, ntree=100)
variableImpPlot(model.rf)
}
