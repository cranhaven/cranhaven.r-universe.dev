\name{piem}
\alias{piem}
\alias{Pi}
\title{
Estimation of the prior probabilities for each association status vector.
}
\description{
The function calls an expectation-maximization (EM) algorithm to estimate the prior probabilities of each association status vector. It is also used internally in \code{\link{repfdr}}.
}
\usage{
piem(pdf.binned.z, binned.z.mat, control = em.control())}
\arguments{
  \item{pdf.binned.z}{Same input as in \code{\link{repfdr}}.
 A 3-dimensional array which contains for each study (first dimension), the probabilities of a z-score to fall in the bin (second dimension), under each hypothesis status (third dimension). The third dimension can be of size 2 or 3, depending on the number of association states: if the association can be either null or only in one direction, the dimension is 2; if the association can be either null, or positive, or negative, the dimension is 3.  
Element \code{[[1]]} in the output of \code{\link{ztobins}}. 
}
  \item{binned.z.mat}{
  Same input as in \code{\link{repfdr}}.
A matrix of the bin numbers for each the z-scores (rows) in each study (columns).
Element \code{[[2]]} in the output of \code{\link{ztobins}}.
}
  \item{control}{
List of control parameters to pass to the EM algorithm. See \code{\link{em.control}}.
}
}
\details{
The implementation of the EM algorithm is in C, and allows paralel processing. By default, the software automatically detects the number of available processing threads. See \code{\link{em.control}} for the option of providing the number of threads to use, as well as for the additional control parameters. 
}
\value{
  \item{all.iterations}{Matrix with number of columns equal to  the number of EM iterations, and each column is the estimated probability distribution of the vector of association status.}
  \item{last.iteration}{Matrix of the vectors of association status along with the column vector of the last EM iteration, which contains the estimated probabilities of the vectors of association status.}
}
\references{
Heller, Ruth, and Daniel Yekutieli. "Replicability analysis for Genome-wide Association studies." \emph{arXiv preprint arXiv:1209.2829} (2012).
}
\author{
\code{C} implementation by Shachar Kaufman.
}
\seealso{
\code{\link{repfdr}}
}
\examples{
\dontrun{

download.file('http://www.math.tau.ac.il/~ruheller/repfdr_RData/binned_zmat.RData',
  destfile = "binned_zmat.RData")
load(file = "binned_zmat.RData")
#binned_zmat can also be generated via 
output_piem <- piem(pbz, bz)

# extract the last iteration to use it in repfdr (see help(repfdr)):
Pi1 <- output_piem$last.iteration
data(Pi)
stopifnot(all.equal(Pi,Pi1))

# simulation data:
data(binned_zmat_sim)
output_piem_sim <- piem(pbz_sim, bz_sim)
Pi_sim <- output_piem_sim$last.iteration

# following are the true proportions in the data: (see help(hmat_sim) for data generation details.)
f <- c(0.895,0.005,0.005,0.02,0.005,0.02,0.02,0.03) 

# the estimation vs the true proportions:
cbind(round(Pi_sim,6),f)
}
}
