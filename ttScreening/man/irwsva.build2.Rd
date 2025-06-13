\name{irwsva.build2}
\alias{irwsva.build2}
\title{
Adjusted irwsva.build which builds surrogate variables from gene expression data
}
\description{
 This function is directly modified from the original irwsva.build() in the SVA package. It was noticed that under certain circumstances a subscript out of bounds error would occur while running the SVA function. Therefore, this modified code has a single line altered that conditionally uses the generic singular decomposition, svd(), instead of fast singular decomposition, fast.svd().
}
\usage{
irwsva.build2(dat, mod, mod0 = NULL, n.sv, B = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
A m CpG sites by n subjects matrix of methylation data.
}
  \item{mod}{
A n by k model matrix corresponding to the primary model fit (see model.matrix)
}
  \item{mod0}{
A n by k0 model matrix corresponding to the null model to be compared to mod.
}
  \item{n.sv}{
The number of surrogate variables to construct.
}
  \item{B}{
The number of iterations of the algorithm to perform.
}
}
\details{
See http://www.bioconductor.org/packages/release/bioc/manuals/sva/man/sva.pdf
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{sv}{A n by n.sv matrix where each column is a distinct surrogate variable.
}
 \item{pprob.gam  }{A vector with the posterior probability estimates that each row is affected by
dependence.
}
\item{pprob.b}{A vector with the posterior probabiliity estimates that each row is affected by
the variables in mod, but not in mod0.
}
\item{n.sv}{The number of suggorate variables estimated.}
}
\references{
Original sva:
Leek JT and Storey JD. (2008) A general framework for multiple testing dependence.Proceedings of the National Academy of Sciences, 105: 18718-18723.

Leek JT and Storey JD. (2007) Capturing heterogeneity in gene expression studies by surrogate variable analysis. PLoS Genetics, 3: e161.
}
\author{
Original irwsva.build: Jeffrey T. Leek <jleek@jhsph.edu>, John Storey jstorey@princeton.edu
}
\note{
sva Vignette http://www.biostat.jhsph.edu/~jleek/sva/
}

