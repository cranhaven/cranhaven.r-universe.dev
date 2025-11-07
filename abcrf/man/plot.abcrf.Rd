\name{plot.abcrf}
\alias{plot.abcrf}
\title{Plot of an ABC-RF object}
\usage{
\method{plot}{abcrf}(x, training, obs=NULL, n.var=20, pdf=FALSE, xlim=NULL, ...)
}
\description{
\code{plot.abcrf} provides both a variable importance plot of a model choice ABC-RF object
and the projection of the reference table on the LDA axes.
}
\arguments{
\item{x}{an \code{abcrf} object.}
\item{training}{the data frame containing the reference table used to train the \code{\link{abcrf}} object.}
\item{obs}{a vector containing the summary statistics of an observed dataset that
will be added to the graph of the projected reference table (black star or vertical line).}
\item{n.var}{number of variables in the variable importance representation.}
\item{pdf}{a boolean that indicates if a pdf version of the graph(s) should be saved in the current directory.}
\item{xlim}{range of the abscissa for the variable importance plot.}
\item{...}{not used.}
}
\note{
The graph of the reference table projected
on the LD axes is shown only if LD axes has
been added to the set of summary statistics
in the call of \code{abcrf}.
}
\references{
Pudlo P., Marin J.-M., Estoup A., Cornuet J.-M., Gautier M. and Robert, C. P. (2016)
\emph{Reliable ABC model choice via random forests} Bioinformatics
\doi{10.1093/bioinformatics/btv684}
}
\seealso{
\code{\link{abcrf}},
\code{\link{predict.abcrf}},
\code{\link{err.abcrf}},
\code{\link{variableImpPlot}}
}
\examples{
data(snp)
modindex <- snp$modindex[1:500]
sumsta <- snp$sumsta[1:500,]
data1 <- data.frame(modindex, sumsta)
model.rf <- abcrf(modindex~., data1, ntree=100)
plot(model.rf, data1)
data(snp.obs)
plot(model.rf, data1, obs=snp.obs[1,])
}
