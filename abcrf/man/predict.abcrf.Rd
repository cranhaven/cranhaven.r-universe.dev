\name{predict.abcrf}
\alias{predict.abcrf}
\title{Predict and evaluate the posterior probability of the MAP
for new data using an ABC-RF object}
\usage{
\method{predict}{abcrf}(object, obs, training, ntree = 1000, 
sampsize = min(1e5,  object$model.rf$num.samples ), paral = FALSE,
ncores = if(paral) max(detectCores()-1,1) else 1, paral.predict = FALSE, 
ncores.predict = if(paral.predict) max(detectCores()-1,1) else 1, ...)
}
\description{
Based on an ABC-RF object this function predicts the best model for
new data and evaluate the posterior probability of the MAP.
}
\arguments{
\item{object}{an \code{\link{abcrf}} object.}
\item{obs}{a data frame containing the summary statistics of the observed data sets.}
\item{training}{the data frame containing the reference table used to train the \code{\link{abcrf}} object.}
\item{ntree}{number of trees to grow in the regression forest, by default 1,000 trees.}
\item{sampsize}{size of the sample from the reference table used to grow a tree
of the forest, by default the minimum between the number of elements of the reference table and 100,000.}
\item{paral}{a boolean that indicates if the calculations of the regression random forest
(forest that returns the posterior probability of the selected model) should be parallelized.}
\item{ncores}{the number of CPU cores to use for the regression random forest construction. If paral=TRUE, it is used the number of CPU cores minus 1. If ncores is not specified and \code{\link{detectCores}} does not detect the number of CPU cores with success then 1 core is used.}
\item{paral.predict}{a boolean that indicates if random forests predictions should be parallelized.}
\item{ncores.predict}{the number of CPU cores to use for random forest predictions (classification and regression). If paral.predict=TRUE, it is used the number of CPU cores minus 1. If ncores.predict is not specified and \code{\link{detectCores}} does not detect the number of CPU cores with success then 1 core is used.}
\item{\dots}{additional arguments to be passed on to \code{\link{ranger}} used to construct
the regression random forest that estimates the posterior probability of the selected model.}
}
\value{
  An object of class \code{abcrfpredict}, which is a list with the
  following components:
  \item{allocation}{indices of the selected models for each observed data set,}
  \item{vote}{votes for each observed dataset,}
  \item{post.prob}{ABC-RF approximations of the posterior probability of the selected model for each observed dataset.}
}
\references{
Pudlo P., Marin J.-M., Estoup A., Cornuet J.-M., Gautier M. and Robert, C. P. (2016)
\emph{Reliable ABC model choice via random forests} Bioinformatics
\doi{10.1093/bioinformatics/btv684}
}
\seealso{
\code{\link{abcrf}},
\code{\link{plot.abcrf}},
\code{\link{err.abcrf}}
}
\examples{
data(snp)
modindex <- snp$modindex[1:500]
sumsta <- snp$sumsta[1:500,]
data1 <- data.frame(modindex, sumsta)
model.rf <- abcrf(modindex~., data1, ntree=100)
data(snp.obs)
predict(model.rf, snp.obs, data1, ntree=100)
}