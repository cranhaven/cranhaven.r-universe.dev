\name{APR.Feature}
\alias{APR.Feature}
\title{
Adjacent position relationship feature.
}
\description{
This feature was proposed by Li \emph{et al}.(2012). In fact this is similar to the PN.FDTF encoding scheme (Huang \emph{et al}., 2006). In this encoding, correlation between adjacent nucleotides  are taken into account.  For any nucleotide sequence with \eqn{n} nucleotides, every two consecutive positions between 1 and \eqn{n}, i.e., \eqn{(1, 2), (2, 3)... (n-1, n)} constitute an APR feature set. For each pair of positions, frequencies of 16 dinucleotides are first computed for both positive and negative dataset, and then the diffence matrix is obtained by substracting the \eqn{16*(n-1)} dinucleotide frequency matrix of positive set from that of negative set. The difference matrix is then be used for encoding of nucleotide sequences. In this encoding procedure each sequence with \eqn{n} nucleotides can be encoded into a vector of \eqn{(n-1)} numeric observations.
}
\usage{
APR.Feature(positive_class, negative_class, test_seq)
}

\arguments{
  \item{positive_class}{
Nucleotide sequence dataset of positive class, must be an object of class \code{\link{DNAStringSet}}.
}
  \item{negative_class}{
Nucleotide sequence dataset of negative class, must be an object of class \code{\link{DNAStringSet}}.
}
  \item{test_seq}{
Nucleotide sequences to be encoded into numeric feature vectors, must be an object of class \code{\link{DNAStringSet}}.
}
}
\details{
The class DNAStringSet can be obtained by using the function \emph{readDNAStringSet} avialble in \strong{Biostrings} package Bioconductor. Here, the sequences must be supplied in FASTA format. Both positive and negative datasets are required for this encoding scheme.
}
\value{
A numeric matrix of order \eqn{m*(n-1)}, where \eqn{m} is the number of sequences in \code{test_seq} and \eqn{n} is the length of sequence.

}
\references{
Li, J.L., Wang, L.F., Wang, H.Y., Bai, L.Y. and Yuan, Z.M. (2012). High-accuracy splice sites prediction based on sequence component and position features. \emph{Genetics and Molecular Research}, 11(3): 3432-3451.
}
\author{
Prabina Kumar Meher, Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}

\seealso{
\code{\link{PN.Fdtf.Feature}}, \code{\link{WAM.Feature}}
}
\examples{
data(droso)
positive <- droso$positive
negative <- droso$negative
test <- droso$test
pos <- positive[1:200]
neg <- negative[1:200]
tst <- test
enc <- APR.Feature(positive_class=pos, negative_class=neg, test_seq=tst)
enc
}

\keyword{Dinucleotide frequency}
\keyword{Nucleotide dependency}
\keyword{Splice sites}
