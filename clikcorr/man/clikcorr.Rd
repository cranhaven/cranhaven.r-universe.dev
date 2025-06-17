\name{clikcorr}
\alias{clikcorr}
\alias{clikcorr.default}
\alias{print.clikcorr}
\alias{summary.clikcorr}

\title{
Censoring data and LIKelihood-based CORRelation estimation and inference}

\description{
A profile likelihood based method of estimation and hypothesis testing on the correlation coefficient of bivariate data with different types of cencoring.
}

\usage{
clikcorr(data, lower1, upper1, lower2, upper2, cp = 0.95, dist = "n", 
 df = 4, sv = NA, nlm = FALSE, \dots)
\method{clikcorr}{default}(data, lower1, upper1, lower2, upper2, cp = 0.95, dist = "n", 
 df = 4, sv = NA, nlm = FALSE, \dots)
\method{print}{clikcorr}(x, \dots)
\method{summary}{clikcorr}(object, \dots)
}

\arguments{
  \item{data}{a data frame name.}
  \item{lower1}{the lower bound of the first of the two variables whose correlation coefficient to be calculated.}
  \item{upper1}{the upper bound of the first of the two variables whose correlation coefficient to be calculated.}
  \item{lower2}{the lower bound of the second of the two variables whose correlation coefficient to be calculated.}
  \item{upper2}{the upper bound of the second of the two variables whose correlation coefficient to be calculated.}
  \item{cp}{confidence level for the confidence interval.}
  \item{dist}{working distribution. By default, \code{dist="n"} assuming the data from a bivariate normal distribution. Set \code{dist="t"} if the data are assumed generated from a bivariate t-distribution.}
  \item{df}{degree of freedom of the bivariate t-distribution when \code{dist="t"}. By default \code{df=4}.}
  \item{sv}{user specified starting values for the vector of (mean1, mean2, var1, corr, var2).}
  \item{nlm}{use \code{nlm} as the optimization method to minimize the negative log (profile) likelihood. By default \code{nlm=FALSE} and \code{optim} is used to maximize the log (profile) likelihood.}
  \item{x}{an object of class \code{"clikcorr"}, i.e., a fitted model.}
  \item{object}{an object of class \code{"clikcorr"}, i.e., a fitted model.}
  \item{\dots}{not used.}
}

\details{\code{clikcorr} conducts point estimation and hypothesis testing on the correlation coefficient of bivariate data with different types of cencoring.}

\value{
   A list with components:
   \item{pairName}{variable names for the input paired data structure in the \code{clikcorr} class.}
   \item{pairData}{a paired data structure in the \code{clikcorr} class.}
   \item{dist}{Normal or t distribution.}
   \item{df}{degree of freedom for t distribution.}
   \item{coefficients}{maximum likelihood estimate (MLE) of the correlation coefficient.}
   \item{Cov}{estimated variance covariance matrix.}
   \item{Mean}{estimated means.}
   \item{CI}{unsymmetric profile confidence interval for the estimated correlation coefficient.}
   \item{P0}{p-value for likelihood ratio test with null hypothesis says that the true correlation coefficient equals zero.}
   \item{logLik}{the value of the log likelihood at MLE.}
}

\references{
Yanming Li, Kerby Shedden, Brenda W. Gillespie and John A. Gillespie (2016). Calculating Profile Likelihood Estimates of the Correlation Coefficient in the Presence of Left, Right or Interval Censoring and Missing Data.
}

\author{
Yanming Li, Kerby Shedden, Brenda W. Gillespie and John A. Gillespie.
}

\examples{

data(ND)
logND <- log(ND)
logND1 <- logND[51:90,]

obj <- clikcorr(logND1, "t1_OCDD", "t2_OCDD", "t1_HxCDF_234678", "t2_HxCDF_234678")

\dontrun{
clikcorr(logND, "t1_OCDD", "t2_OCDD", "t1_HxCDF_234678", "t2_HxCDF_234678")

clikcorr(logND, "t1_OCDD", "t2_OCDD", "t1_HxCDF_234678", "t2_HxCDF_234678",
 nlm=TRUE)

clikcorr(logND, "t1_OCDD", "t2_OCDD", "t1_HxCDF_234678", "t2_HxCDF_234678",
 method="BFGS")

clikcorr(logND, "t1_OCDD", "t2_OCDD", "t1_HxCDF_234678", "t2_HxCDF_234678",
 sv=c(5,-0.5,0.6,0.5,0.6))

clikcorr(logND, "t1_OCDD", "t2_OCDD", "t1_HxCDF_234678", "t2_HxCDF_234678",
 dist="t", df=10, nlm=TRUE)
}

print(obj)
summary(obj)

}

