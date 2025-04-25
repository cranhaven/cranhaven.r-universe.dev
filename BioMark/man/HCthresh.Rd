\name{HCthresh}
\alias{HCthresh}
\title{Biomarker thresholding by Higher Criticism}
\description{
  Higher Criticism (HC) is a second-level significance testing approach
  to determine which variables in a multivariate set show significant
  differences in two classes. Function \code{HCthresh} selects those p
  values that are significantly different from what would be expected
  from their uniform distribution under the null hypothesis.
}
\usage{
HCthresh(pvec, alpha = 0.1, plotit = FALSE)
}
\arguments{
  \item{pvec}{Vector of p values.}
  \item{alpha}{Parameter of the HC approach: the maximal fraction of
    differentially expressed p values.}
  \item{plotit}{Logical, whether or not a plot should be produced.}
}
\details{
  In HC, one tests the deviation of the expected behaviour of p values
  under a null distribution. Function \code{HCthresh} implements the
  approach by Donoho and Jin to find out which of these correspond to
  real differences. The prerequisites are that the true biomarkers are
  rare (consist of only a small fraction of all variables) and weak (are
  not able to discriminate between the two classes all by themselves).
}
\value{A vector containing the ordered
  indices of the p values satisfying the HC criterion. 
}
\references{
  David Donoho and Jiashun Jin: Higher criticism thresholding: Optimal
  feature selection when useful features are rare and weak. \emph{PNAS}
  108:14790-14795 (2008).

  Ron Wehrens and Pietro Franceschi: Thresholding for Biomarker
  Selection in Multivariate Data using Higher Criticism.
  Mol. Biosystems (2012). In press. DOI: 10.1039/C2MB25121C
}
\author{Ron Wehrens}

\seealso{
  \code{\link{get.biom}} for general approaches to obtain biomarkers
  based on multivariate discriminant methods and t statistics
}
\examples{
data(spikedApples)
bms <- get.biom(spikedApples$dataMatrix, rep(0:1, each = 10),
                type = "coef", fmethod = "studentt")
bms.pvalues <- 2 * (1 - pt(abs(bms[[1]]), 18))
sum(bms.pvalues < .05)                           ## 15
sum(p.adjust(bms.pvalues, method = "fdr") < .05) ## 4
signif.bms <- HCthresh(bms.pvalues, plotit = TRUE)
length(signif.bms)                               ## 11
}
\keyword{htest}
\keyword{models}
