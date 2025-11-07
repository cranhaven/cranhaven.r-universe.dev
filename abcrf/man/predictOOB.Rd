\name{predictOOB}
\alias{predictOOB}
\alias{predictOOB.regAbcrf}
\title{
Predict out-of-bag posterior expectation, median, variance, quantiles and error measures using a reg-ABC-RF object
}
\description{
Based on a reg-ABC-RF object this function predicts the out-of-bag posterior expectation, median, variance, quantiles, mean squared errors, normalized mean absolute errors, credible interval and coverage, for the corresponding parameter using the out-of-bag observations of the training data set.

Mean squared errors and normalized mean absolute errors are computed both with mean and median of the response variable.

Memory allocation issues might be encountered when the size of the training data set is large.
}
\usage{
\method{predictOOB}{regAbcrf}(object, training, quantiles=c(0.025,0.975), paral = FALSE,
ncores = if(paral) max(detectCores()-1,1) else 1,...)
}
\arguments{
  \item{object}{a \code{\link{regAbcrf}} object.}
  \item{training}{the data frame containing the reference table used to train the \code{\link{regAbcrf}} object.}
  \item{quantiles}{numeric vector of probabilities with values in [0,1]. The default value is equal to \code{c(0.025, 0.975)}.}
  \item{paral}{a boolean that indicates if training data predictions should be parallelized or not.}
  \item{ncores}{the number of CPU cores to use for the regression random forest predictions. If paral=TRUE, it is used the number of CPU cores minus 1. If ncores is not specified and \code{\link{detectCores}} does not detect the number of CPU cores with success then 1 core is used.}
  \item{\dots}{optional arguments to be passed on to the function \code{\link{predict.ranger}}.}
}

\value{
An object of class \code{regAbcrfOOBpredict}, which is a list with the following components:
\item{expectation}{predicted posterior expectation for each oberved data set,}
\item{med}{predicted posterior median for each oberved data set,}
\item{variance}{predicted posterior variance for each observed data set, computed by reusing weights,}
\item{variance.cdf}{predicted posterior variance for each observed data set, computed by approximation of the cumulative distribution function,}
\item{quantiles}{predicted posterior quantiles for each observed data set,}
\item{MSE}{mean squared error computed with prediction based on mean of response variable,}
\item{NMAE}{normalized mean absolute error computed with predictions based on mean of response variable,}
\item{MSE.med}{mean squared error computed with predictions based on median of response variable,}
\item{NMAE.med}{normalized mean absolute error with predictions based on median of response variable,}
\item{coverage}{credible inteval coverage if only two quantiles are of interest, NULL otherwise.}
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
res <- predictOOB(model.rf.r, data2) 
}