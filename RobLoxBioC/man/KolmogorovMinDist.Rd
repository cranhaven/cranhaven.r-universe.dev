\name{KolmogorovMinDist}
\alias{KolmogorovMinDist}
\alias{KolmogorovMinDist-methods}
\alias{KolmogorovMinDist,matrix,Norm-method}
\alias{KolmogorovMinDist,AffyBatch,AbscontDistribution-method}
\alias{KolmogorovMinDist,beadLevelData,AbscontDistribution-method}

\title{Generic Function for Computing Minimum Kolmogorov Distance for Biological Data}
\description{
  Generic function for computing minimum Kolmogorov distance for biological data.
}
\usage{
KolmogorovMinDist(x, D, ...)

\S4method{KolmogorovMinDist}{matrix,Norm}(x, D, mad0 = 1e-4)

\S4method{KolmogorovMinDist}{AffyBatch,AbscontDistribution}(x, D, bg.correct = TRUE, pmcorrect = TRUE, 
                  verbose = TRUE)

\S4method{KolmogorovMinDist}{beadLevelData,AbscontDistribution}(x, D, log = FALSE, what = "Grn", 
                  probes = NULL, arrays = NULL)
}
\arguments{
  \item{x}{ biological data. }
  \item{D}{ object of class \code{"UnivariateDistribution"}. }
  \item{\dots}{ additional parameters. }
  \item{mad0}{ scale estimate used if computed MAD is equal to zero. 
    Median and MAD are used as start parameter for optimization. }
  \item{bg.correct}{ if \code{TRUE} MAS 5.0 background correction is performed;
    confer \code{\link[affy:bgc]{bg.correct.mas}}. }
  \item{pmcorrect}{ if \code{TRUE} \code{log2(PM/MM)} is used. 
    If \code{FALSE} only \code{log2(PM)} is used. }
  \item{verbose}{ logical: if \code{TRUE}, some messages are printed. }
  \item{log}{ if \code{TRUE}, then the log2 intensities for each bead-type are summarized. }
  \item{what}{ character string specifying which intensities/values to summarize; 
    see \code{\link[beadarray]{getBeadData}}. }
  \item{probes}{ Specify particular probes to summarize. If left \code{NULL} then all
    the probes on the first array are used. }
  \item{arrays}{ integer (scalar or vector) specifying the strips/arrays to summarize. 
    If \code{NULL}, then all strips/arrays are summarized. }
}
\details{
  The minimum Kolmogorov distance is computed for each row of a matrix, each
  Affymetrix probe, or each Illumina bead, respectively.
  
  So far, only the minimum distance to the set of normal distributions can be
  computed.
}
\value{ List with components \code{dist} containing a numeric vector 
or matrix with minimum Kolmogorov distances and \code{n} a numeric vector 
or matrix with the corresponding sample sizes. }
\references{
  Huber, P.J. (1981) \emph{Robust Statistics}. New York: Wiley.

  Rieder, H. (1994) \emph{Robust Asymptotic Statistics}. New York: Springer.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link[distrEx]{KolmogorovDist}}, \code{\link[distrMod]{MDEstimator}} }
\examples{
set.seed(123) # to have reproducible results for package checking

## matrix method for KolmogorovMinDist
ind <- rbinom(200, size=1, prob=0.05) 
X <- matrix(rnorm(200, mean=ind*3, sd=(1-ind) + ind*9), nrow = 2)
KolmogorovMinDist(X, D = Norm())

## using Affymetrix data
data(SpikeIn)
probes <- log2(pm(SpikeIn))
(res <- KolmogorovMinDist(probes, Norm()))
boxplot(res$dist)

\donttest{
## \donttest because of check time
## using Affymetrix data
library(affydata)
data(Dilution)
res <- KolmogorovMinDist(Dilution[,1], Norm())
summary(res$dist)
boxplot(res$dist)
plot(res$n, res$dist, pch = 20, main = "Kolmogorov distance vs. sample size",
     xlab = "sample size", ylab = "Kolmogorov distance",
     ylim = c(0, max(res$dist)))
uni.n <- min(res$n):max(res$n)
lines(uni.n, 1/(2*uni.n), col = "orange", lwd = 2)
legend("topright", legend = "minimal possible distance", fill = "orange")

## Illumina bead level data
library(beadarrayExampleData)
data(exampleBLData)
res <- KolmogorovMinDist(exampleBLData, Norm(), arrays = 1)
res1 <- KolmogorovMinDist(exampleBLData, Norm(), log = TRUE, arrays = 1)
summary(cbind(res$dist, res1$dist))
boxplot(list(res$dist, res1$dist), names = c("raw", "log-raw"))
sort(unique(res1$n))
plot(res1$n, res1$dist, pch = 20, main = "Kolmogorov distance vs. sample size",
     xlab = "sample size", ylab = "Kolmogorov distance",
     ylim = c(0, max(res1$dist)), xlim = c(min(res1$n), 56))
uni.n <- min(res1$n):56
lines(uni.n, 1/(2*uni.n), col = "orange", lwd = 2)
legend("topright", legend = "minimal possible distance", fill = "orange")
}
}
\concept{normal location and scale}
\concept{minimum distance}
\keyword{robust}
