\name{lasso_dsp_boot}
\alias{lasso_dsp_boot}
\title{
Estimate GGMs with the desparsified lasso using the bootstrap.
}
\description{
Estimate Gaussian Graphical Models using the desparsified lasso using the bootstrap. This is a wrapper around the function \code{lasso.proj} of the \code{hdi} package.
}
\usage{
    lasso_dsp_boot(data, betainit = "cv lasso", ci.level = 0.95,
                   correction = TRUE, B = 1000, pbar = TRUE,
                   rulereg = "and")
}
\arguments{
  \item{data}{
An n x p matrix containing the data, where n are cases and p are variables
}
  \item{betainit}{
Specifying how to estimate lasso solution in initial estimation. Either \code{betainit = "cv lasso"} (default) or \code{betainit = "cv lasso"}. See the manual of the function \code{lasso.proj} of the \code{hdi} package for more info.
}
  \item{ci.level}{
Specifies the width of the confidence interval used for testing the null hypothesis that a parameter is different to zero. Defaults to \code{ci.level = 0.95}, which corresponds to a critical threshold of 0.05.
}
  \item{correction}{
If \code{correction = TRUE}, the Bonferroni-Holm correction will be applied to p-values on the level of nodewise regressions (see e.g., Hochberg, 1987).
}
  \item{B}{
The number of bootstrap samples used for estimation. Defaults to \code{B=1000}.
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

Bühlmann, P., Kalisch, M., & Meier, L. (2014). High-dimensional statistics with a view toward applications in biology. Annual Review of Statistics and Its Application, 1, 255-278.

Davison, A. C., & Hinkley, D. V. (1997). Bootstrap methods and their application (No. 1). Cambridge university press.
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
out <- lasso_dsp_boot(data = data, B=2)
# !!! NOTE: this is just for testing purposes; B should be a lot higher (default = 1000)

\dontrun{

# Fit GGM to PTSD data
set.seed(1)
out <- lasso_dsp_boot(data = ptsd_data)

}

}

