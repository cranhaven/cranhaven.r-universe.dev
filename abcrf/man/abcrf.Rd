\name{abcrf}
\alias{abcrf}
\alias{abcrf.formula}

\title{Create an ABC-RF object: a classification random forest from a reference table towards performing an ABC model choice}
\description{
\code{abcrf} constructs a random forest from a reference table towards performing
an ABC model choice. Basically, the reference table (i.e. the dataset that will
be treated with the present package) includes a column with the index
of the models to be compared and additional columns corresponding
to the values of the simulated summary statistics.
}
\usage{
\method{abcrf}{formula}(formula, data, group=list(), lda=TRUE, ntree=500, sampsize=min(1e5, nrow(data)),
paral=FALSE, ncores= if(paral) max(detectCores()-1,1) else 1, ...)
}
\arguments{
\item{formula}{a formula: left of ~, variable representing
the model index; right of ~, summary statistics of the reference table.}
\item{data}{a data frame containing the reference table.}
\item{group}{a list containing groups (at least 2) of model(s)
on which the model choice will be performed. This is not necessarily a partition,
one or more models can be excluded from the elements of the list and
by default no grouping is done.}
\item{lda}{should LDA scores be added to the list of summary statistics?}
\item{ntree}{number of trees to grow in the forest, by default 500 trees.}
\item{sampsize}{size of the sample from the reference table to grow a tree
of the classification forest, by default the minimum between the number of elements of the reference table and 100,000.}
\item{paral}{a boolean that indicates if the calculations of the classification random forest
(forest used to assign a model to the observed dataset) should be parallelized.}
\item{ncores}{the number of CPU cores to use. If paral=TRUE, it is used the number of CPU cores minus 1. If ncores is not specified and \code{\link{detectCores}} does not detect the number of CPU cores with success then 1 core is used.}
\item{\dots}{additional arguments to be passed on to \code{\link{ranger}} used to construct the classification random forest that preditcs the selected model.}
}
\value{
An object of class \code{abcrf}, which is a list with the
following components:
\item{call}{the original call to \code{abcrf},}
\item{lda}{a boolean indicating if LDA scores have been added to the list of summary statistics,}
\item{formula}{the formula used to construct the classification random forest,}
\item{group}{a list contining the groups of model(s) used. This list is empty if no grouping has been performed,}
\item{model.rf}{an object of class \code{randomForest} containing the trained forest with the reference table,}
\item{model.lda}{an object of class \code{lda} containing the Linear Discriminant Analysis based on the reference table,}
\item{prior.err}{prior error rates of model selection on the reference table, estimated with the "out-of-bag" error of the forest.}
}
\references{
Pudlo P., Marin J.-M., Estoup A., Cornuet J.-M., Gautier M. and Robert, C. P. (2016)
\emph{Reliable ABC model choice via random forests} Bioinformatics
\doi{10.1093/bioinformatics/btv684}

Estoup A., Raynal L., Verdu P. and Marin J.-M. (2018)
\emph{Model choice using Approximate Bayesian Computation and Random Forests: analyses based on model grouping to make inferences about the genetic history of Pygmy human populations} Jounal de la Société Française de Statistique
\url{http://journal-sfds.fr/article/view/709}
}
\seealso{
\code{\link{plot.abcrf}},
\code{\link{predict.abcrf}},
\code{\link{err.abcrf}},
\code{\link{ranger}}
}
\examples{
data(snp)
modindex <- snp$modindex[1:500]
sumsta <- snp$sumsta[1:500,]
data1 <- data.frame(modindex, sumsta)
model.rf1 <- abcrf(modindex~., data = data1, ntree=100)
model.rf1
model.rf2 <- abcrf(modindex~., data = data1, group = list(c("1","2"),"3"), ntree=100)
model.rf2
}
