\name{hyprbg}
\alias{hyprbg}
\title{
Hypergeometric Test (phyper)
}
\description{
Performs Hypergeometric test (phyper() from R) 
}
\usage{
hyprbg(Sig_in_Paths, uniSig, gns_in_Paths, universe)
}
\arguments{
  \item{Sig_in_Paths}{
Number of significant genes in the pathway
}
  \item{uniSig}{
Number of significant genes in the dataset
}
  \item{gns_in_Paths}{
Number of genes in the pathway
}
  \item{universe}{
Number of genes in the dataset
}
}
\value{
Returns hypergeometric test
}
\references{
hyprbg Imports phyper() (from stats)
}