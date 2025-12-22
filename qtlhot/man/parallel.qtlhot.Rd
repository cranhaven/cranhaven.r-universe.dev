\name{parallel.qtlhot}
\alias{parallel.qtlhot}
\alias{qtlhot.phase0}
\alias{big.phase0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Code for parallelizing R/qtlhot.
}
\description{
Code for parallelizing R/qtlhot. See installed parallel directory for
proper use. There is apparently an S3 parallel method, so doc has to be
as shown below, even though it is called as parallel.qtlhot.
}
\usage{
\method{parallel}{qtlhot}(x, data = 1, \dots, dirpath = ".")
qtlhot.phase0(dirpath, init.seed = 92387475, len = rep(400,16), n.mar = 185, n.ind = 112,
   n.phe = 100, latent.eff = 0, res.var = 1, lod.thrs, \dots)
big.phase0(dirpath, cross, trait.file, trait.matrix, droptrait.names = NULL,
   keeptrait.names = NULL, lod.thrs, sex = "Sex", trait.index,
   batch.effect = NULL, size.set = 250, offset = 0, subset.sex = NULL, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{phase of parallel processing (1,2,3)}
  \item{data}{index for parallel processing (1,2,...)}
  \item{\dots}{additional arguments passed along}
  \item{dirpath}{directory path as character string}
  \item{init.seed}{initial seed for pseudorandom number generation}
  \item{len}{vector of chromosome lengths for simulated map}
  \item{n.mar}{number of markers for simulated map}
  \item{n.ind}{number of individuals for simulated cross}
  \item{n.phe}{number of phenotypes for simulated phenotypes}
  \item{latent.eff}{size of latent effect}
  \item{res.var}{magnitude of residual variance}
  \item{lod.thrs}{vector of LOD thresholds to examine}
  \item{cross}{object of class \code{cross}}
  \item{trait.file}{character string name of trait file}
  \item{trait.matrix}{character string name of trait matrix}
  \item{droptrait.names}{vector of character strings for traits to drop
    (none if \code{NULL})}
  \item{keeptrait.names}{vector of character strings for traits to keep
    (keep all if \code{NULL})}
  \item{sex}{character string name of phenotype for sex}
  \item{trait.index}{vector of character strings for trait names}
  \item{batch.effect}{character string for batch effect (none if \code{NULL})}
  \item{size.set}{maximum size of set of traits to scan at one time}
  \item{offset}{offset for name of trait RData files}
  \item{subset.sex}{string of sex to subset on (both sexes if \code{NULL})}
  \item{verbose}{verbose output if \code{TRUE}}
}
\seealso{\code{\link[qtl]{read.cross}}}
\author{
Brian S Yandell and Elias Chaibub Neto
}
\keyword{utilities}
