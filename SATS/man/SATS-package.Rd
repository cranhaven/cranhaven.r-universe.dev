
\name{SATS-package}
\alias{SATS}
\docType{package}
\title{
    SATS (Signature Analyzer for Targeted Sequencing)
}
\description{
  This package is created to perform mutational signature 
  analysis for targeted sequenced tumors. 
  Unlike the canonical analysis of mutational signatures, 
  SATS factorizes the mutation counts matrix into a panel 
  context matrix (measuring the size of the targeted sequenced genome 
  for each tumor in the unit of million base pairs (Mb)), 
  a signature profile matrix, and a signature activity matrix. 
  SATS also calculates the expected number of mutations attributed 
  by a signature, namely signature burden, 
  for each targeted sequenced tumor.
}
\details{
This package includes a novel algorithm, SATS, 
to perform mutational signature analysis for targeted 
sequenced tumors. The algorithm first applies the signeR 
algorithm to extract profiles of de novo mutational signatures 
by appropriately adjusting for various panel sizes. 
Next, the profiles of identified de novo mutational signatures 
are mapped to the profiles of catalog signatures of tumor mutation 
burden (TMB), in the unit of the number of mutations per million base pairs, 
using penalized non-negative least squares. 
Then, given the panel sizes and profiles of mapped TMB catalog signatures, 
signature activities are estimated for all samples simultaneously 
through the Expectation-Maximization (EM) algorithm. 
Finally, the expected number of mutations attributed by a signature, 
namely signature burden, is calculated for each targeted sequenced tumor.

The main functions in this package are \code{\link{GenerateLMatrix}}, 
\code{\link{EstimateSigActivity}},\cr 
\code{\link{CalculateSignatureBurdens}}, and \code{\link{MappingSignature}}.
}
\references{
Lee, D., Hua, M., Wang, D., Song, L., Yu, K., Yang, X., Shi, J., Landi, M., 
Zhu, B. The mutational signatures of 100,477 targeted sequenced tumors. Submitted.
}
\author{Donghyuk Lee <dhyuklee@pusan.ac.kr> and Bin Zhu <bin.zhu@nih.gov>}
\keyword{package}
