\name{sva2}
\alias{sva2}
\title{
The adjusted sva code using irwsva.build2
}
\description{
This function is the modified SVA function in which it uses the irwsva.build2 function rather than the irwsva.build function to build the surrogate variables. Thus, only a single line has been altered from the original SVA() function.
}
\usage{
sva2(dat, mod, mod0 = NULL, n.sv = NULL, method = c("irw", "two-step"), 
	vfilter = NULL, B = 5, numSVmethod = "be")
}

\arguments{
  \item{dat}{
An m by n (m cpg sites by n subjects) matrix of methylation data.
}
  \item{mod}{
A n by k model matrix corresponding to the primary model fit (see model.matrix).
}
  \item{mod0}{
A n by k0 model matrix corresponding to the null model to be compared to mod.
}
  \item{n.sv}{
Optional. The number of surrogate variables to estimate, can be estimated using the num.sv function.
}
  \item{method}{
Choose between the iteratively re-weighted or two-step surrogate variable estimation algorithms.
}
  \item{vfilter}{
The number of most variable CpG sites to use when building SVs, must be between 100 and m.
}
  \item{B}{
The number of iterations of the algorithm to perform.
}
  \item{numSVmethod}{
The method for determining the number of surrogate variables to use.
}
}
\details{
 See http://www.bioconductor.org/packages/release/bioc/manuals/sva/man/sva.pdf
}
\value{
  \item{sv }{
A n by n.sv matrix where each column is a distinct surrogate variable.
}
 \item{pprob.gam }{
A vector with the posterior probability estimates that each row is affected by dependence.
}
\item{pprob.b}{
A vector with the posterior probabiliity estimates that each row is affected by the variables in mod, but not in mod0.
}
\item{n.sv}{
The number of suggorate variables estimated.
}
}
\references{
Original sva:
Leek JT and Storey JD. (2008) A general framework for multiple testing dependence.       Proceedings of the National Academy of Sciences, 105: 18718-18723. 

Leek JT and Storey JD. (2007) Capturing heterogeneity in gene expression studies by surrogate variable analysis. PLoS Genetics, 3: e161.

}
\author{
Original sva: Jeffrey T. Leek <jleek@jhsph.edu>, John Storey jstorey@princeton.edu
}
\note{
sva Vignette http://www.biostat.jhsph.edu/~jleek/sva/
}
