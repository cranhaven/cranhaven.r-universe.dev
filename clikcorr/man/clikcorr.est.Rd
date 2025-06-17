\name{est}
\alias{est}

\title{
censoring data and likelihood-based correlation estimation
}
\description{
Provides point estimation and confidence interval for the correlation coefficient.
}
\usage{
est(data, lower1, upper1, lower2, upper2, cp = 0.95, dist = "n", df = 4, sv = NA, 
nlm = FALSE, \dots)
}

\arguments{
  \item{data}{data frame name.}
  \item{lower1}{the lower bound of the first of the two variables whose correlation coefficient to be calculated.}
  \item{upper1}{the upper bound of the first of the two variables whose correlation coefficient to be calculated.}
  \item{lower2}{the lower bound of the second of the two variables whose correlation coefficient to be calculated.}
  \item{upper2}{the upper bound of the second of the two variables whose correlation coefficient to be calculated.}
  \item{cp}{confidence level for the confidence interval.}
  \item{dist}{working distribution. By default, \code{dist="n"} assuming the data from a bivariate normal distribution. Set \code{dist="t"} if the data are assumed generated from a bivariate t-distribution.}
  \item{df}{degree of freedom of the bivariate t-distribution when \code{dist="t"}. By default \code{df=4}.}
  \item{sv}{user specified starting values for the vector of (mean1, mean2, var1, corr, var2).}
  \item{nlm}{use \code{nlm} as the optimization method to minimize the negative log (profile) likelihood. By default \code{nlm=FALSE} and \code{optim} is used to maximize the log (profile) likelihood.}
  \item{\dots}{not used.}
}

\value{
   \item{Cor}{maximum likelihood estimate (MLE) of the correlation coefficient.}
   \item{Cov}{estimated variance covariance matrix.}
   \item{Mean}{estimated means.}
   \item{LCL}{lower bound of the profile confidence interval.}
   \item{UCL}{upper bound of the profile confidence interval.}
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

est(logND1, "t1_OCDD", "t2_OCDD", "t1_HxCDF_234678", "t2_HxCDF_234678")

\dontrun{
est(logND, "t1_TCDD", "t2_TCDD", "t1_PeCDD", "t2_PeCDD")

est(logND, "t1_TCDD", "t2_TCDD", "t1_PeCDD", "t2_PeCDD", dist="t",
 nlm=TRUE)
}
}

