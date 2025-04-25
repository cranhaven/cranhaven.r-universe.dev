\name{ROC}
\alias{ROC}
\alias{print.ROC}
\alias{plot.ROC}
\alias{lines.ROC}
\alias{points.ROC}
\alias{identify.ROC}
\alias{ROC.default}
\alias{roc.value}
\alias{AUC}
\title{ROC curves}
\description{Functions for making, plotting and analysing ROC curves.}
\usage{
ROC(TestResult, \dots)
\method{ROC}{default}(TestResult, D, take.abs = TRUE, \dots)
\method{plot}{ROC}(x, type = "b", null.line = TRUE,
xlab = "False Pos. Rate", ylab = "True Pos. Rate",
xlim = c(0, 1), ylim = c(0, 1), main = "ROC", \dots) 
\method{points}{ROC}(x, \dots)
\method{lines}{ROC}(x, \dots)
\method{identify}{ROC}(x, labels = NULL, \dots, digits = 1)
\method{print}{ROC}(x, \dots)
roc.value(found, true, totalN)
AUC(x, max.mspec)
}
\arguments{
  \item{TestResult}{Typically regression coefficients or t
    statistics. Note that when p values are used directly, the least
    significant values would be selected first. In this case one should
    use 1/p.}
  \item{D}{True, known, differences, either expressed as a vector of 0
    and 1 of the same length as \code{TestResult} or as a vector of
    indices.}
  \item{take.abs}{Logical, indicating whether to take absolute values of
    the test statistic.}
  \item{x}{An object of class ROC.}
  \item{type, xlab, ylab, xlim, ylim, main, labels, digits}{Standard
    arguments to functions like \code{plot} and \code{identify}.}
  \item{null.line}{Logical, whether to draw the line y = x,
    corresponding to random guessing.}
  \item{max.mspec}{Maximal value of the True Positive Rate to consider
    in AUC calculations. Setting this to a value smaller than one (which
    is the default) leads to a partial AUC value, which may in many
    cases be more useful.}
  \item{found}{The indices of the coefficients identified with a
    biomarker identification method.}
  \item{true}{The indices of the true biomarkers.}
  \item{totalN}{The total number of variables to choose from.}
  \item{\dots}{Further arguments, especially useful in the plotting
    functions.}
}
\value{
  Function \code{ROC} returns a list with elements:
  \enumerate{
    \item{sens}{Sensitivity, or True Positive Rate (TPR).}
    \item{mspec}{1 - Specificity, or False Positive Rate (FPR).}
    \item{test}{levels of the test statistic.}
    \item{call}{Function call.}
  }
  
Function \code{roc.value} returns a list with elements \code{sens} and
\code{mspec}, i.e., one point on a ROC curve.

Function \code{AUC} returns the area under the curve, measured up to the
value of \code{max.mspec} - if the latter is smaller than 1, it is a
partial AUC curve.
}

\references{
T. Lumley: ROC curves - in Programme's Niche, R News 4/1, June 2004.
}
\author{Ron Wehrens}
\examples{
data(spikedApples)
apple.coef <- get.biom(X = spikedApples$dataMatrix,
                       Y = rep(1:2, each = 10),
                       fmethod = "vip",
                       ncomp = 3, type = "coef")

## ROC curve for all VIP values, ordered according to size
true.biom <- (1:ncol(spikedApples$dataMatrix) \%in\% spikedApples$biom)
vip.roc <- ROC(apple.coef$vip, true.biom)
plot(vip.roc)

## Add stability-based selection point
apple.stab <- get.biom(X = spikedApples$dataMatrix,
                       Y = rep(1:2, each = 10),
                       fmethod = "vip",
                       ncomp = 3, type = "stab")
stab.roc <- roc.value(apple.stab$vip[[1]]$biom.indices,
                      spikedApples$biom,
                      totalN = ncol(spikedApples$dataMatrix))
points(stab.roc, col = "red", pch = 19, cex = 1.5)

\dontrun{
## Add HC-based selection point
## Attention: takes approx. 2 minutes on my PC
apple.HC <- get.biom(X = spikedApples$dataMatrix,
                     Y = rep(1:2, each = 10),
                     fmethod = "vip",
                     ncomp = 3, type = "HC")
HC.roc <- roc.value(apple.HC$vip$biom.indices,
                    spikedApples$biom,
                    totalN = ncol(spikedApples$dataMatrix))
points(HC.roc, col = "blue", pch = 19, cex = 1.5)
}
}
\keyword{classif}
