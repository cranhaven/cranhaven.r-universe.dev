\name{sv}
\alias{sv}

\title{
Calculating starting values for the vector of (mean1, mean2, var1, corr, var2) from completely observed data. 
}
\description{
Calculates starting values for the vector of (mean1, mean2, var1, corr, var2) from completely observed data. 
}

\usage{
sv(data, lower1, upper1, lower2, upper2)
}

\arguments{
  \item{data}{a data frame name.}
  \item{lower1}{the lower bound of the first variable of the two variables whose correlation coefficient to be calculated.}
  \item{upper1}{the upper bound of the first variable of the two variables whose correlation coefficient to be calculated.}
  \item{lower2}{the lower bound of the second variable of the two variables whose correlation coefficient to be calculated.}
  \item{upper2}{the upper bound of the second variable of the two variables whose correlation coefficient to be calculated.}
}

\details{
function \code{sv} calculates starting values for the vector of (mean1, mean2, var1, corr, var2) from completely observed data. 
}

\value{
  \item{mu1}{starting value for the mean parameter of the first variable.}
  \item{mu2}{starting value for the mean parameter of the second variable.}
  \item{var1}{starting value for the variance parameter of the first variable.}
  \item{cor}{starting value for the correlation coefficient.}
  \item{var2}{starting value for the variance parameter of the second variable.}
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

sv(logND, "t1_TCDD", "t2_TCDD", "t1_PeCDD", "t2_PeCDD")

}
