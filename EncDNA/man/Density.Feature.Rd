\name{Density.Feature}
\alias{Density.Feature}
\title{
Nucleotide sequence encoding with the distribution of trinucleotides.
}
\description{
Each nucleotide sequence is encoded into a numeric vector of same length based on the distribution of nucleotides over the sequence. Here, two classes of dataset are not required for encoding, and each sequence is independently encoded instead. This encoding seheme was introduced by Wei \emph{et al}. (2013) for prediction of donor and acceptor human splice sites along with the \code{MM1.Feature}.
}
\usage{
Density.Feature(test_seq)
}
\arguments{
  \item{test_seq}{
Sequence dataset to be encoded, must be an object of class \code{\link{DNAStringSet}}.
}
}
\details{
The class DNAStringSet can be obtained by reading FASTA sequences using the function \emph{readDNAStringSet} avialble in \strong{Biostrings} package of Bioconductor.
}
\value{
A numeric matrix of order \eqn{m*n}, where \eqn{m} is the number of sequences in \code{test_seq} and \eqn{n} is the length of sequence.
}
\references{
Bari, A.T.M.G., Reaz, M.R. and Jeong, B.S. (2014). Effective DNA encoding for splice site prediction using SVM. \emph{MATCH Commun. Math. Comput. Chem.}, 71: 241-258.
}
\author{
Prabina Kumar Meher, Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}

\examples{
data(droso)
test <- droso$test
tst <- test[1:5]
enc <- Density.Feature(test_seq=tst)
enc
}
\keyword{Splice sites}
\keyword{Sequence encoding}
