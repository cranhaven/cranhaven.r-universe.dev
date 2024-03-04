\name{snphamming}
\alias{snphamming}
\title{
  Diffusion kernels on SNP genotypes 
}
\description{
  This function construct a diffusion kernel on a \eqn{p}-dimensional
  SNP hamming graph, where each genotype takes on three possible configurations, namely 0
  ('aa'), 1 ('Aa'), and 2('AA'). This graph is obained by the
  \eqn{p}-Cartesian graph product of a complete graph \eqn{K_3}. 
  It contains \eqn{3^p} vertices corresponding to sequences of
  genotypes, and two vertices are adjacent if and only if just one SNP
  locus differs. 
}
\usage{
snphamming(X, theta)
}
\arguments{
  \item{X}{A genotype matrix of \eqn{n} individuals with \eqn{p} SNPs (\eqn{n \times p}).
  }
  \item{theta}{The rate of diffusion. 
  }
}
\value{
  Diffusion kernel matrix of size \eqn{n \times n}.
  This can be viewed as a covariance among individuals given the
  diffusion rate. 
}
\references{
  Kondor R and Lafferty J: (2002) Diffusion Kernels on Graphs and Other Discrete Input Spaces. ICML. 

  Morota G, Koyama M, Rosa GJM, Weigel KA, and Gianola D. (2013). Predicting complex traits using a diffusion kernel on genetic markers with an application to dairy cattle and wheat data. Genetics Selection Evolution. 45:17. 
}
\author{
  Gota Morota \email{morota@unl.edu} and Masanori Koyama \email{koyama.masanori@gmail.com}
}
\seealso{
  \code{\link{snpgrid}}
}
\examples{
# set a seed
set.seed(4321)

# create a genotype matrix of 5 individuals with 10 SNPs
X <- matrix(sample(c(0,1,2), 50, prob=c(0.35, 0.3, 0.35), replace=TRUE),
ncol=10)

# set the rate of diffusion equal to 1
theta <- 1

# compute a SNP hamming kernel 
snphamming(X, theta)
}
\keyword{graphs}
\keyword{nonparametric}
