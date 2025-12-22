\name{hotperm}
\alias{hotperm}
\alias{print.hotperm}
\alias{summary.hotperm}
\alias{quantile.hotperm}
\alias{print.summary.hotperm}
\alias{hotperm1}
\title{Conduct NL and N permutation tests}
\description{
Conduct NL and N permutation tests.
}
\usage{
hotperm(cross, n.quant, n.perm, lod.thrs, alpha.levels, drop.lod = 1.5,
  window = NULL, verbose = FALSE, init.seed = 0,
  addcovar = NULL, intcovar = NULL, \dots)
data(hotperm1)
\method{print}{hotperm}(x, \dots)
\method{summary}{hotperm}(object, quant.levels, \dots)
\method{quantile}{hotperm}(x, probs, \dots, lod.thr = NULL)
\method{print}{summary.hotperm}(x, \dots)
}
\arguments{
  \item{cross}{object of class \code{cross}}
  \item{n.quant}{maximum of \code{s.quant}}
  \item{n.perm}{number of permutations}
  \item{lod.thrs}{vector of LOD thresholds}
  \item{alpha.levels}{vector of significance levels}
  \item{quant.levels}{quantile levels, as number of traits, to show in
    summary; default is 1, 2, 5, 10, \dots up to maximum recorded}
  \item{drop.lod}{LOD drop amount for support intervals}
  \item{window}{window size for smoothed hotspot size}
  \item{verbose}{verbose output if \code{TRUE}}
  \item{init.seed}{initial seed for pseudo-random number generation}
  \item{x,object}{object of class \code{hotperm} or \code{summary.hotperm}}
  \item{probs}{probability levels for quantiles (\code{1-probs} if all > 0.5);
    default is \code{alpha.levels}}
  \item{lod.thr}{restrict to values above this if not \code{NULL}}
  \item{addcovar}{additive covariates as vector or matrix; see \code{\link[qtl]{scanone}}}
  \item{intcovar}{interactive covariates as vector or matrix; see \code{\link[qtl]{scanone}}}
  \item{\dots}{arguments passed along to \code{scanone}}
}
\author{
  Elias Chaibub Neto and Brian S Yandell
}
\examples{
example(include.hotspots)
set.seed(123)
pt <- scanone(ncross1, method = "hk", n.perm = 1000)
alphas <- seq(0.01, 0.10, by=0.01)
lod.thrs <- summary(pt, alphas)
\dontrun{
## This takes awhile, so we save the object.
set.seed(12345)
hotperm1 <- hotperm(cross = cross1,
                    n.quant = 300,
                    n.perm = 100,
                    lod.thrs = lod.thrs,
                    alpha.levels = alphas,
                    drop.lod = 1.5,
                    verbose = FALSE)
save(hotperm1, file = "hotperm1.RData", compress = TRUE)
}
summary(hotperm1)
}
\keyword{utilities}
