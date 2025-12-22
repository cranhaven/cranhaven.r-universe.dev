\name{sim.hotspot}
\alias{sim.hotspot}
\alias{mySimulations}
\alias{sim.null.cross}
\alias{sim.null.pheno.data}
\alias{include.hotspots}
\title{Wrapper routine for simulations.}
\description{
Wrapper routine for simulations
}
\usage{
sim.hotspot(nSim, cross, n.pheno, latent.eff, res.var = 1, n.quant, n.perm,
  alpha.levels, lod.thrs, drop.lod = 1.5, verbose = FALSE)
mySimulations(\dots)
sim.null.cross(chr.len = rep(400, 16), n.mar = 185, n.ind = 112,
  type = "bc", n.pheno = 6000, latent.eff = 1.5, res.var = 1,
  init.seed = 92387475)
sim.null.pheno.data(cross, n.pheno, latent.eff, res.var)
include.hotspots(cross, hchr, hpos, hsize, Q.eff, latent.eff,
  lod.range.1, lod.range.2, lod.range.3, res.var=1, n.pheno, init.seed)
}
\arguments{
  \item{nSim}{Number of simulated sets of phenotypes to create. See details.}
  \item{cross}{Object of class \code{cross}. See \code{\link[qtl]{read.cross}}.}
  \item{n.pheno}{Number of traits, or phenotypes, to simulate for cross object.}
  \item{latent.eff}{Strength of latent effect, which is included in all traits. See \code{\link{sim.null.cross}}.}
  \item{res.var}{Residual variance for traits. Should not affect results.}
  \item{n.quant}{maximum size of hotspots examined; ideally large enough to exceed the largest Breitling alpha critical value.}
\item{n.perm}{Number of permutations to perform per realization. Good idea to do 1000,
    but this takes time.}
  \item{alpha.levels}{Vector of significance levels.}
  \item{lod.thrs}{Vector of LOD thresholds, typically single-trait
    permutation thresholds for various significance levels.}
  \item{drop.lod}{Drop in LOD score examined. LODs below this drop from the maximum for a
    chromosome will not be scored.}
  \item{init.seed}{initial seed for pseudo-random number generation}
  \item{chr.len}{vector of chromosome lengths}
  \item{n.mar}{number of markers}
  \item{n.ind}{number of individuals}
  \item{type}{type of cross}
  \item{hchr,hpos,hsize}{vectors for hotspot chromosomes, positions, and
  sizes}
  \item{Q.eff}{QTL effect}
  \item{lod.range.1,lod.range.2,lod.range.3}{2-vectors of LOD ranges for
  multiple purposes}
  \item{verbose}{Verbose output if \code{TRUE}. More detailed output if \code{2}.}
  \item{\dots}{Arguments passed directly to \code{sim.hotspot}.}
}
\details{
Simulate \code{nSim} realizations of cross object with \code{n.pheno} phenotypes with correlation
\code{latent.eff}. All simulations use the same genotypes in the
\code{cross} object.
}
\value{
  \code{sim.null.cross} simulates an object of class \code{cross}.
  \code{sim.null.pheno.data} simulates a data frame of phenotypes.
  \code{sim.hotspot} uses these other routines to simulate a hotspot,
  returning an list object.
}
\author{
Elias Chaibub Neto and Brian S. Yandell
}
\seealso{
\code{\link{sim.null.cross}}, \code{\link[qtl]{read.cross}}.
}
\examples{
ncross1 <- sim.null.cross(chr.len = rep(100, 4),
                          n.mar = 51,
                          n.ind = 100,
                          type = "bc",
                          n.phe = 1000,
                          latent.eff = 3,
                          res.var = 1,
                          init.seed = 123457)
cross1 <- include.hotspots(cross = ncross1,
                           hchr = c(2, 3, 4),
                           hpos = c(25, 75, 50),
                           hsize = c(100, 50, 20),
                           Q.eff = 2,
                           latent.eff = 3,
                           lod.range.1 = c(2.5, 2.5),
                           lod.range.2 = c(5, 8),
                           lod.range.3 = c(10, 15),
                           res.var = 1,
                           n.phe = 1000,
                           init.seed = 12345)
}
\keyword{utilities}
