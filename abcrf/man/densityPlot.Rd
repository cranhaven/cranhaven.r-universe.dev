\name{densityPlot}
\alias{densityPlot}
\alias{densityPlot.regAbcrf}
\title{Plot the posterior density given a new summary statistic}
\description{
Given a reg-ABC-RF object and a new value of the summary statistics, 
\code{densityPlot} gives the corresponding posterior density plot of the parameter, as well as the prior (in grey).
}
\usage{
\method{densityPlot}{regAbcrf}(object, obs, training,
add=TRUE, main="Posterior density", log="", xlim=NULL,
ylim=NULL, xlab=NULL, ylab=NULL, paral=FALSE,
ncores= if(paral) max(detectCores()-1,1) else 1, ...)
}
\arguments{
  \item{object}{a \code{\link{regAbcrf}} object.}
  \item{obs}{a data frame containing the summary statistics of the observed data sets.}
  \item{training}{the data frame containing the reference table used to train the \code{\link{regAbcrf}} object.}
  \item{add}{a boolean that indicates if the posterior distributions should be ploted on the same graph or not, when more than one observed summary statistics is given.}
  \item{main}{main title to be used for the posterior density plot.}
  \item{log}{a character string which contains "x" if the x axis is to be logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if both axes are to be logarithmic. The default value "" implies no logarithmic transfomation.}
  \item{xlim}{range of the abscissa.}
  \item{ylim}{range of the ordinate.}
  \item{xlab}{label of the abscissa.}
  \item{ylab}{label of the ordinate.}
  \item{paral}{a boolean that indicates if random forests predictions should be parallelized.}
  \item{ncores}{the number of CPU cores to use for the regression random forest predictions. If paral=TRUE, it is used the number of CPU cores minus 1. If ncores is not specified and \code{\link{detectCores}} does not detect the number of CPU cores with success then 1 core is used.}
  \item{\dots}{additional arguments to be passed on to \code{\link{density}}, as for example the smoothing bandwidth \code{bw} to be used.}
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
\code{\link{covRegAbcrf}},
\code{\link{ranger}},
\code{\link{plot.regAbcrf}}
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
densityPlot(model.rf.r, snp.obs, data2, ylab="density", main = "Posterior density of r")
}
