\name{predict.regAbcrf}
\alias{predict.regAbcrf}
\title{
Predict posterior expectation, median, variance and quantiles
given a new dataset using a reg-ABC-RF object
}
\description{
Based on a reg-ABC-RF object this function predicts the posterior expectation, median, variance, quantiles for the corresponding parameter given new dataset. Somes posterior errors can be 
computed at an higher computational price.
}
\usage{
\method{predict}{regAbcrf}(object, obs, training, quantiles=c(0.025,0.975),
paral = FALSE, ncores = if(paral) max(detectCores()-1,1) else 1, rf.weights = FALSE,
post.err.med = FALSE, ...)
}
\arguments{
  \item{object}{a \code{\link{regAbcrf}} object.}
  \item{obs}{a data frame containing the summary statistics of the observed data sets.}
  \item{training}{the data frame containing the reference table used to train the \code{\link{regAbcrf}} object.}
  \item{quantiles}{numeric vector of probabilities with values in [0,1]. The default value is equal to \code{c(0.025, 0.975)}.}
  \item{paral}{a boolean that indicates if random forests predictions should be parallelized.}
  \item{ncores}{the number of CPU cores to use for the regression random forest predictions. If paral=TRUE, it is used the number of CPU cores minus 1. If ncores is not specified and \code{\link{detectCores}} does not detect the number of CPU cores with success then 1 core is used.}
  \item{rf.weights}{a boolean that indicates if the random forest weights used to predict quantities of interest should we returned. The default value is FALSE.}
  \item{post.err.med}{a boolean that indicates if posterior errors based on posterior medians should be computed. The default value is FALSE. If computed, this function might take a much more time.}
  \item{\dots}{optional arguments to be passed on to the function \code{\link{predict.ranger}}.}
}
\value{
An object of class \code{regAbcrfpredict}, which is a list with the following components:
\item{expectation}{predicted posterior expectation for each oberved data set,}
\item{med}{predicted posterior median for each oberved data set,}
\item{variance}{predicted posterior variance for each observed data set, computed by reusing weights, this quantity is also the posterior mean squared error,}
\item{variance.cdf}{predicted posterior variance for each observed data set, computed by approximation of the cumulative distribution function,}
\item{quantiles}{predicted posterior quantiles for each observed data set,}
\item{weights}{a matrix composed of the weights used to predict quantities of interest. Returned if \code{rf.weights} is TRUE,}
\item{post.NMAE.mean}{posterior normalized mean absolute error obtained using the out-of-bag posterior expectation (mean) and previously computed random forest weights, for each observed data set,}
\item{post.MSE.med}{posterior mean squared error obtained using the out-of-bag posterior median and previously computed random forest weights, for each observed data set,}
\item{post.NMAE.med}{posterior normalized mean absolute error obtained using the out-of-bag posterior expectation (mean) and previously computed random forest weights, for each observed data set,}
\item{prior.MSE}{prior mean squared error computed with training out-of-bag prediction based on mean of response variable,}
\item{prior.NMAE}{prior normalized mean absolute error computed with training out-of-bag predictions based on mean of response variable,}
\item{prior.MSE.med}{prior mean squared error computed with training out-of-bag predictions based on median of response variable,}
\item{prior.NMAE.med}{prior normalized mean absolute error with training out-of-bag predictions based on median of response variable,}
\item{prior.coverage}{prior credible inteval coverage computed for training instances, if only two quantiles are of interest, NULL otherwise.}
}
\references{
Raynal L., Marin J.-M. Pudlo P., Ribatet M., Robert C. P. and Estoup, A. (2019)
\emph{ABC random forests for Bayesian parameter inference} Bioinformatics
\doi{10.1093/bioinformatics/bty867}
}
\seealso{
\code{\link{regAbcrf}},
\code{\link{predictOOB}},
\code{\link{plot.regAbcrf}},
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
data(snp.obs)
predict(model.rf.r, snp.obs, data2)
}