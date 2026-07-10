\name{OLS}
\alias{OLS}

\title{
Estimate GMM via nodewise regression and hypothesis tests.
}
\description{
Estimate Gaussian Graphical Model with nodewise regression, selecting edges with standard hypothesis tests and the Bonferroni-Holm Correction.
}
\usage{
OLS(data, pbar = TRUE, correction = TRUE,
    ci.level = 0.95, rulereg = "and")
}

\arguments{
  \item{data}{
An n x p matrix containing the data, where n are cases and p are variables
}
  \item{pbar}{
If \code{pbar = TRUE}, a progress bar will be displayed.
}
  \item{correction}{
If \code{correction = TRUE}, the Bonferroni-Holm correction will be applied to p-values on the level of nodewise regressions (see e.g., Hochberg, 1987).
}
  \item{ci.level}{
Specifies the width of the confidence interval used for testing the null hypothesis that a parameter is different to zero. Defaults to \code{ci.level = 0.95}, which corresponds to a critical threshold of 0.05.
}
  \item{rulereg}{
Specifies how parameter estimates should be combined across nodewise regressions. The options are the AND-rule (requiring both estimates to be significant) or the OR-rule (only requiring one estimate to be significant). Defaults to \code{rulereg = "and"}.
}
}

\value{
The function returns a list with the following entries:

\item{est}{A p x p matrix with point estimates for all partial correlations}
\item{est.signf}{A p x p matrix with point estimates for all partial correlations with non-significant partial correlations being thresholded to zero.}
\item{signf}{A p x p matrix indicating for each partial correlation whether it is significantly different to zero.}
\item{ci.lower}{A p x p matrix indicating the lower confidence interval for each partial correlation.}
\item{ci.upper}{A p x p matrix indicating the upper confidence interval for each partial correlation.}
\item{ints}{A p-vector of estimated intercepts.}

}
\references{
Hochberg, Y., & Tamhane, A. C. (1987). Multiple comparison procedures. John Wiley & Sons, Inc..
}
\author{
Jonas Haslbeck <jonashaslbeck@gmail.com>
}

\examples{

# Toy example that runs relatively quickly
library(MASS)
p <- 5 # number of variables
data <- mvrnorm(n=100, mu=rep(0, p), Sigma = diag(p))
out <- OLS(data = data)

\dontrun{

# Fit GGM to PTSD data
out <- OLS(data = ptsd_data)

}

}
