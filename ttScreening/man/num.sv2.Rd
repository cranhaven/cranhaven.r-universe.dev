\name{num.sv2}
\alias{num.sv2}

\title{
Adjusted num.sv which estimates the number of important surrogate variables from a gene expression data set.
}
\description{
This function is directly modified from the orginal num.sv() in the \pkg{sva} package. This function has the tolerance level in the fast.svd() function set back to its orginal default instead of 0.
}
\usage{
num.sv2(dat, mod, method = c("be", "leek"), vfilter = NULL, 
B = 20, sv.sig = 0.1, seed = NULL)
}
\arguments{
  \item{dat}{
A m genes by n arrays matrix of expression data.
}
  \item{mod}{
A n by k model matrix corresponding to the primary model fit (see model.matrix).
}
  \item{method}{
The method to use for estimating surrogate variables, for now there is only one option (based ib Buja and Eyuboglu 1992).
}
  \item{vfilter}{
The number of most variable genes to use when building SVs, must be between 100 and m.
}
  \item{B}{
The number of null iterations to perform. Only used when method="be".
}
  \item{sv.sig}{
The significance cutoff for eigengenes. Only used when method="be".
}
  \item{seed}{
A numeric seed for reproducible results. Optional, only used when method="be".
}
}
\details{
See http://www.bioconductor.org/packages/release/bioc/manuals/sva/man/sva.pdf
}
\value{
\item{n.sv}{The number of significant surrogate variables}
}
\references{
Original sva:
Leek JT and Storey JD. (2008) A general framework for multiple testing dependence.Proceedings of the National Academy of Sciences, 105: 18718-18723.

Leek JT and Storey JD. (2007) Capturing heterogeneity in gene expression studies by surrogate variable analysis. PLoS Genetics, 3: e161.
}
\author{
Original num.sv: Jeffrey T. Leek <jleek@jhsph.edu>, John Storey jstorey@princeton.edu
}

\note{
sva Vignette http://www.biostat.jhsph.edu/~jleek/sva/
}
