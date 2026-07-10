\name{lasso_ms}
\alias{lasso_ms}
\title{
Estimate GMM with inference via the multi-split method.
}
\description{
Estimate Gaussian Graphical Models with inference base don the multi-split method. This is a wrapper of the function \code{multi.split} of the \code{hdi} package.
}
\usage{
lasso_ms(data, B = 50, fraction = 0.5, ci.level = 0.95,
         correction = TRUE, pbar = TRUE, rulereg = "and")
}
\arguments{
  \item{data}{
An n x p matrix containing the data, where n are cases and p are variables
}
  \item{B}{
The number of sample-splits. Defaults to \code{B=50}.
}
  \item{fraction}{
a number in (0,1), the fraction of data used at each sample split for the model selection process. The remaining data is used for calculating the p-values.
}
  \item{ci.level}{
Specifies the width of the confidence interval used for testing the null hypothesis that a parameter is different to zero. Defaults to \code{ci.level = 0.95}, which corresponds to a critical threshold of 0.05.
}
  \item{correction}{
If \code{correction = TRUE}, the Bonferroni-Holm correction will be applied to p-values on the level of nodewise regressions (see e.g., Hochberg, 1987).
}
  \item{pbar}{
If \code{pbar = TRUE}, a progress bar will be displayed.
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

}
\references{
Hochberg, Y., & Tamhane, A. C. (1987). Multiple comparison procedures. John Wiley & Sons, Inc..

Wasserman, L., & Roeder, K. (2009). High dimensional variable selection. Annals of statistics, 37(5A), 2178.

Meinshausen, N., Meier, L., & Bühlmann, P. (2009). P-values for high-dimensional regression. Journal of the American Statistical Association, 104(488), 1671-1681.
}
\author{
Jonas Haslbeck <jonashaslbeck@gmail.com>; Lourens Waldorp <waldorp@uva.nl>
}

\examples{

# Toy example that runs relatively quickly
library(MASS)
p <- 5 # number of variables
data <- mvrnorm(n=100, mu=rep(0, p), Sigma = diag(p))
set.seed(1)
out <- lasso_ms(data = data, B=2)
# !!! NOTE: this is just for testing purposes; B should a lot higher (default = 50)

\dontrun{

# Fit GGM to empirical PTSD data
set.seed(1)
out <- lasso_ms(data = ptsd_data)

}

}
