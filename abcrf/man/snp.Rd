\name{snp}
\alias{snp}
\alias{snp.obs}
\title{A simulated example in population genetics}

\description{
The simulated example of population genetics with SNP loci used in Pudlo et al.
(2016): \code{snp} contains the reference table on which to perform ABC model choice, it also contains the simulated parameters to perform regression random forest.
\code{snp.obs} contains two pseudo-observed data sets. The first one
(\code{favorable}) should be easily allocated to a model, while that is not the case for the second one (\code{unfavorable}).
}
\usage{
data(snp)
data(snp.obs)
}
\format{
\code{snp} is a list containing an ABC reference table of 10,000
simulations from a Bayesian prior predictive model (see Pudlo et al., 2016, for a description of the model choice issue). The first element, named \code{modindex} is a \code{factor} containing the model indices, the second element, \code{param}, is a data frame with seven simulated parameters. The last element of this list, named \code{sumsta}, contains the reference table on which to perform ABC model choice and parameter estimation.

\code{snp.obs} is a data frame containing the summary statistics of two pseudo-observed data sets.
}
\source{
Pudlo, P., Marin, J.-M., Estoup, A., Cornuet, J.-M., Gautier, M. and Robert, C.P. (2016)
\emph{Reliable ABC model choice via random forests} Bioinformatics
\doi{10.1093/bioinformatics/btv684}
}
\examples{
data(snp)
data(snp.obs)
}
