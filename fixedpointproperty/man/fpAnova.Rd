\name{fpAnova}
\alias{fpAnova}
\title{
Function to perform ANOVA on fp objects
}
\description{
This function computes Bayes Factors and p-values for within-subjects ANOVA designs, encoded as fp objects.}
\usage{
fpAnova(object, stat = "BF", na.rm = TRUE, check = TRUE)
}
\arguments{
  \item{object}{a list of objects from class fpp.
}
  \item{stat}{Either "BF" (default), "p", or "both", specificying what statistic to report.
}
  \item{na.rm}{Are \code{NA}s removed?
}
  \item{check}{Should the data be checked for suitability? A warning will be provided if a check is failed.
  }
}
\details{
The function expects the output of \code{fpGet}, but in a list. 
}
\value{
A list containing the results of either the Bayesian or frequentist analysis, or both:

\item{BF }{The output of \code{anovaBF}}
\item{p }{The output of \code{summary.aov}}
}
\references{
Van Maanen, L., De Jong, R., Van Rijn, H (2014). How to assess the existence of competing strategies in cognitive tasks: A primer on the fixed-point property. PLOS One, 9, e106113

Van Maanen, L. Couto, J. & Lebetron, M. (2016). Three boundary conditions for computing the fixed-point property in binary mixture data. PLOS One, 11, e0167377.
}
\author{
Leendert van Maanen  (l.vanmaanen@uu.nl)}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning }{
If \code{check=TRUE}, then warnings will be provided if the data are not suitable for correct inferences.
}

\seealso{
\code{\link{fpGet}}, \code{\link{anovaBF}}, \code{\link{fpConditionCheck}}, \code{\link{summary.aov}}.
}
\examples{
## generate data
p <- c(.1,.5,.9)
rt <- sapply(1:3, function(i) {rnormMix(10000, c(1,2), c(1,1), p[i])})
dat <- data.frame(rt=c(rt), cond=rep(1:3, each=10000), pp=rep(1:50, each=200, times=3))

## compute the list of fpp objects
res <- tapply(1:nrow(dat), dat$pp, function(X) {fpGet(dat[X,], 1000, bw=.75)})

## call fpAnova, with stat="both" to do both a Bayesian and a frequentist test
fpAnova(res, stat="both")
}