\name{mtcmim}
\alias{mtcmim}
\title{
MTCMIM
}
\description{
Multiple-trait composite multiple-interval mapping.
}
\usage{
mtcmim(y, mpos, mdat, x, xid, dists, a, b, sigma, qtl=NULL,
   eps=NULL, win=Inf, range=0, pp=1, len=2, init=1,
   iter=10000,tol=1e-12)
}
\arguments{
  \item{y}{
a n by p matrix, whose columns are dependent variables.
}
  \item{mpos}{
a data frame (id=marker index, ch=chromosome id, m=marker index on the chromosome, dist=genetic position in cM on the chromosome). Chromosome id should be an integer.
}
  \item{mdat}{
a matrix of n rows; marker genotypes (1 or 0). Columns should correspond to markers in the order.
}
  \item{x}{
covariates; n by m numerical matrix.
}
  \item{xid}{
a list of length p, xid[[j]] specifies columns of x as covariates for y[,j] .
}
  \item{dists}{
a data frame (ch=chromosome id, mid=marker index, d=genetic position in cM on the chromosome); specifies initial QTL locations.  Chromosome id should be an integer.
}
  \item{a}{
initial covariate effects including intercepts (if given).
}
  \item{b}{
initial QTL effects (if given).
}
  \item{sigma}{
initial residual variance-covariance (if given).
}
  \item{qtl}{
a list of length p (if given); qtl[[j]] specifies qtls for y[,j], which are defined by rows of \code{dists}.
}
  \item{eps}{
a data frame (y=which trait,q1=QTL one,q2=QTL two) (if not \code{NULL}); specifies epistatic terms.
}
  \item{win}{
window width of search around existing QTL. Ignored if \code{range=0}.
}
  \item{range}{
search range: genome-wide (0), the same chromosomes (-1).
}
  \item{pp}{
mapping population: BC-1, RIL-selfing-2, RIL-brother-sister-mating-3.
}
  \item{len}{
step length in search.
}
  \item{init}{
whether \code{a}, \code{b} and \code{sigma} are used as initial values.
}
  \item{iter}{
maximum number of iterations in a numerical process to estimate model parameters.
}
  \item{tol}{
convergence tolerance.
}
}

\details{
Given the covariates, the number of QTL and epistasis that are specified for each trait, this function searches for the optimal genomic locations of the QTL, and estimates the model parameters.
}

\value{
a list with the following components:
  \item{loglik}{log-likelihood of the final model}
  \item{a}{covariate effects}
  \item{b}{QTL effects}
  \item{sigma}{residual variance-covariance}
  \item{qtl}{QTL for each trait}
  \item{eps}{epistatic terms}
  \item{dists}{QTL locations}
}

\examples{
data(etrait)
qtl<- vector("list",16); qtl[[1]]<- c(1,2)
eps<- data.frame(y=1,q1=1,q2=2)
dists<- dists[c(4,11),]
x<- mdat - 3/2
\dontrun{
o<- mtcmim(traits, mpos, mdat, x, xid, dists, qtl=qtl, eps=eps,
   win=5, range=-1, pp=2, len=1)
}
}

