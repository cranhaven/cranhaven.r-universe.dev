\name{highlod}
\alias{highlod}
\alias{print.highlod}
\alias{summary.highlod}
\alias{plot.highlod}
\alias{max.highlod}
\alias{quantile.highlod}
\alias{pull.highlod}
\title{
Pull high LOD values with chr and pos.
}
\description{
Pull high LOD values with chr and pos.
}
\usage{
highlod(scans, lod.thr = 0, drop.lod = 1.5,
  extend = TRUE, restrict.lod = FALSE, \dots)
pull.highlod(object, chr, pos, \dots)
\method{print}{highlod}(x, \dots)
\method{summary}{highlod}(object, \dots)
\method{plot}{highlod}(x, \dots, quant.level = NULL, sliding = FALSE)
\method{max}{highlod}(x, lod.thr = NULL, window = NULL, quant.level = NULL, \dots)
\method{quantile}{highlod}(x, probs = NULL, lod.thr = NULL, n.quant,
  n.pheno, max.quantile = TRUE, \dots)
}
\arguments{
  \item{scans}{object of class \code{\link[qtl]{scanone}}}
  \item{lod.thr}{LOD threshold}
  \item{drop.lod}{LOD drop from max to keep for support intervals}
  \item{extend}{extend support interval just past \code{drop.lod};
    matches behavior of \code{\link[qtl]{lodint}} when \code{TRUE}}
  \item{restrict.lod}{restrict to loci above LOD threshold if
    \code{TRUE}; matches behavior of \code{\link[qtl]{lodint}} when
    \code{FALSE} (default)}
  \item{chr}{chromosome identifier}
  \item{pos}{position, or range of positions, in cM}
  \item{x,object}{object of class \code{highlod}}
  \item{probs}{probability levels for quantiles (should be > 0.5)}
  \item{n.quant}{maximum of \code{s.quant}}
  \item{n.pheno}{number of phenotypes considered}
  \item{max.quantile}{return only quantiles of max LOD across genome if
    \code{TRUE}}
  \item{window}{size of window for smoothing hotspot size}
  \item{quant.level}{vector of LOD levels for 1 up to
    \code{length(quant.level)} size hotspots}
  \item{sliding}{plot as sliding hotspot if \code{TRUE}}
  \item{\dots}{arguments passed along}
}
\seealso{\code{\link{highlod}}, \code{\link{hotperm}}}
\value{
  Data frame with
  \item{row}{row number in \code{\link[qtl]{scanone}} object}
  \item{phenos}{phenotype column number}
  \item{lod}{LOD score for phenotype at locus indicated by \code{row}} 
}
\details{
  The \code{highlod} condenses a \code{scanone} object to the peaks
  above a \code{lod.thr} and/or within \code{drop.lod} of such
  peaks. The \code{pull.highlod} pulls out the entries at a particular
  genomic location or interval of locations. Summary, print, plot, max
  and quantile methods provide ways to examine a \code{highlod} object.
}
\author{
Brian S Yandell and Elias Chaibub Neto
}
\examples{
example(include.hotspots)
scan1 <- scanone(cross1, pheno.col = 1:1000, method = "hk")
high1 <- highlod(scan1, lod.thr = 2.11, drop.lod = 1.5)
pull.highlod(high1, chr = 2, pos = 24)
summary(high1, lod.thr = 2.44)
max(high1, lod.thr = seq(2.11, 3.11, by = .1))
}
\keyword{utilities}
