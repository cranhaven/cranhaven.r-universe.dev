\name{lrt}
\alias{lrt}

\title{
censoring data and likelihood-based correlation estimation inference
}
\description{
Provides likelihood ratio tests for making statistical inference about the correlation coefficient from bivariate censored/missing data.
}
\usage{
lrt(data, lower1, upper1, lower2, upper2, dist = "n", df = 4, 
 sv = NA, r0 = 0, nlm = FALSE, \dots)
}

\arguments{
  \item{data}{a data frame name.}
  \item{lower1}{the lower bound of the first of the two variables whose correlation coefficient to be calculated.}
  \item{upper1}{the upper bound of the first of the two variables whose correlation coefficient to be calculated.}
  \item{lower2}{the lower bound of the second of the two variables whose correlation coefficient to be calculated.}
  \item{upper2}{the upper bound of the second of the two variables whose correlation coefficient to be calculated.}
  \item{dist}{working distribution. By default, \code{dist="n"} assuming the data from a bivariate normal distribution. Set \code{dist="t"} if the data are assumed generated from a bivariate t-distribution.}
  \item{df}{degree of freedom of the bivariate t-distribution when \code{dist="t"}. By default \code{df=4}.}
  \item{sv}{user specified starting values for the vector of (mean1, mean2, var1, corr, var2).}
  \item{r0}{correlation coefficient value under the null hypothesis. By default is 0.}
  \item{nlm}{use \code{nlm} as the optimization method to minimize the negative log (profile) likelihood. By default \code{nlm=FALSE} and \code{optim} is used to maximize the log (profile) likelihood.}
  \item{\dots}{not used.}
}

\value{
   \item{Cor}{maximum likelihood estimate (MLE) of the correlation coefficient.}
   \item{m1llk}{value of the log likelihood function evaluated at the MLE.}
   \item{m0llk}{value of the log likelihood function evaluated at the r0.}
   \item{P0}{p-value for likelihood ratio test with null hypothesis says that the true correlation coefficient equals r0.}
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

lrt(logND, "t1_TCDD", "t2_TCDD", "t1_PeCDD", "t2_PeCDD")

\dontrun{
lrt(logND, "t1_TCDD", "t2_TCDD", "t1_PeCDD", "t2_PeCDD", dist="t")
}
}

