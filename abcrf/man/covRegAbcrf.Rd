\name{covRegAbcrf}
\alias{covRegAbcrf}
\alias{covRegAbcrf.regAbcrf}
\title{
Predict posterior covariance between two parameters for new data using two reg-ABC-RF objects
}
\description{
Using two reg-ABC-RF objects constructed on the same reference table for two different response variables, this function predicts the posterior covariance between those two response variables, given a new dataset of summaries.
}
\usage{
\method{covRegAbcrf}{regAbcrf}(regForest1, regForest2, obs, training1, training2,
ntree=500, mtry=max(floor((dim(training1)[2]-1)/3), 1),
sampsize=min(1e5, dim(training1)[1]), paral = FALSE, 
ncores = if(paral) max(detectCores()-1,1) else 1, paral.predict = FALSE,
ncores.predict = if(paral.predict) max(detectCores()-1,1) else 1, ... )
}
\arguments{
  \item{regForest1, regForest2}{\code{\link{regAbcrf}} objects.}
  \item{obs}{a data frame containing the summary statistics of the observed data sets.}
  \item{training1, training2}{data frames containing the reference table respectively used to train the \code{\link{regAbcrf}} objects regForest1 and regForest2.}
  \item{ntree}{number of trees to grow in the forest, by default equal to 500 trees.}
  \item{mtry}{Number of variables to possibly split at in each node for the regression random forest. Default is the minimum between 1 and the number of variables divided by 3.}
  \item{sampsize}{size of the sample from the reference table used to grow a tree of the regression forest, by default the minimum between the number of elements of the reference table and 100,000.}
  \item{paral}{a boolean that indicates whether or not the calculations of the regression random forest (forest used to predict a response from the observed dataset) should be parallelized.}
  \item{ncores}{the number of CPU cores to use. If paral=TRUE, it is used the number of CPU cores minus 1. If ncores is not specified and \code{\link{detectCores}} does not detect the number of CPU cores with success then 1 core is used.}
  \item{paral.predict}{a boolean that indicates if random forests predictions should be parallelized.}
  \item{ncores.predict}{the number of CPU cores to use for the regression random forest predictions. If paral.predict=TRUE, it is used the number of CPU cores minus 1. If ncores.predict is not specified and \code{\link{detectCores}} does not detect the number of CPU cores with success then 1 core is used.}
  \item{\dots}{additional arguments to be passed on to \code{\link{ranger}} used to construct the regression random forest that predicts posterior covariance.}
}
\value{
\code{covRegAbcrf} returns predicted posterior covariances between response variables of two reg-ABC-RF objects, for a new data set.
}
\references{
Raynal L., Marin J.-M. Pudlo P., Ribatet M., Robert C. P. and Estoup, A. (2019)
\emph{ABC random forests for Bayesian parameter inference} Bioinformatics
\doi{10.1093/bioinformatics/bty867}
}
\seealso{
\code{\link{regAbcrf}},
\code{\link{predict.regAbcrf}},
\code{\link{err.regAbcrf}},
\code{\link{plot.regAbcrf}},
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
N1 <- snp$param$N1[modindex == "3"]
N1 <- N1[1:500]
data3 <- data.frame(N1, sumsta) 
model.rf.N1 <- regAbcrf(N1~., data3, ntree=100)
data(snp.obs)
covRegAbcrf(model.rf.r, model.rf.N1, snp.obs, 
            data2, data3, ntree=100)
}
