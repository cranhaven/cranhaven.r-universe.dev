\name{ww.perm}
\alias{ww.perm}
\alias{summary.ww.perm}
\alias{print.ww.perm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Conduct West-Wu (Q) permutation tests}
\description{
Conduct West-Wu (Q) permutation tests.
}
\usage{
ww.perm(highobj, n.perm, lod.thrs, alpha.levels, verbose = FALSE)
\method{print}{ww.perm}(x, \dots)
\method{summary}{ww.perm}(object, alpha.levels, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{highobj}{object of class \code{\link{highlod}}}
  \item{n.perm}{number of permutations}
  \item{lod.thrs}{vector of LOD thresholds}
  \item{alpha.levels}{vector of significance levels}
  \item{x,object}{object of class \code{ww.perm}}
  \item{\dots}{ignored}
  \item{verbose}{verbose output if \code{TRUE}}
}
\author{
Elias Chaibub Neto and Brian S Yandell
}
\details{
Perform permutation tests to assess the statistical significance of the
hotspots detected using the West-Wu \code{Q}-method permutations. The
\code{ww.perm} function implements the \code{Q}-method's permutation
scheme (see the Method's section of Chaibub Neto et a. 2012, for
details). The \code{n.perm} parameter specifies the number of
simulations. Here we set it to 100 in order to save time. In practice,
we recommend at least 1,000 permutations. The function's output is a
matrix with 100 rows representing the permutations, and 10 columns
representing the QTL mapping thresholds. Each entry \code{ij}, represents the
maximum number of significant linkages across the entire genome detected
at permutation \code{i}, using the LOD threshold \code{j}. The
\code{ww.summary} function computes the Q-method's hotspot size
permutation thresholds, that is, the \code{1-alpha} quantiles for each
one of the QTL mapping LOD thrsholds in \code{lod.thrs}. For instance,
the entry at row 10 and column 1 of the \code{Q.1.thr} matrix tells us
that the 99\% percentile of the permutation distribution of genome wide
maximum hotspot size based on a QTL mapping threshold of 2.11 is
27.00. In other words, any hotspot greater than 27 is considered
statistically significant at a 0.01 significance level when QTL mapping
is done using a 2.11 LOD threshold. 
In general, we are often interested in using the same error rates for
the QTL mapping and hotspot analysis. That is, if we adopt a QTL mapping
threshold that controls GWER at a 1\% level (in our case, 3.11) we will
also want to consider \code{alpha = 0.01} for the hotspot analysis,
leading to a hotspot threshold of 12.00. Therefore, we are usually more
interested in the diagonal of \code{Q.1.thr}. We adopted a GWER of 5\%,
and the corresponding \code{Q}-method's permutation threshold is
18. According to this threshold, all hotspots are significant. 
}
\examples{
\dontrun{
## All unspecified objects come from vignette qtlhot.
set.seed(12345)
Q.1 <- ww.perm(high1, n.perm = 100, lod.thrs, alphas)
Q.1.thr <- summary(Q.1, alphas)
Q.1.thr
diag(Q.1.thr)

set.seed(12345)
Q.2 <- ww.perm(high2, 100, lod.thrs, alphas)
Q.2.thr <- summary(Q.2, alphas)
}
}
\keyword{utilities}
