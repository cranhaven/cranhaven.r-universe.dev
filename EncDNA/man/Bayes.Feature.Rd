\name{Bayes.Feature}
\alias{Bayes.Feature}
\title{
Projecting nucleotide sequences into numeric feature vectors using Bayes kernel encoding approach.
}
\description{
This sequence encoding technique was introduced by Zhang \emph{et al}. (2006) for prediction of splice sites. In this encoding technique, positional frequencies of nucleotides are computed for both positive and negative datasets, which are then used for encoding of any nucleotide sequence of same length. Each sequence of length \emph{L} can be encoded into a numeric feature vector of length \emph{2L}. Both positive and negative classes of sequences are required for sequence encoding.
}
\usage{
Bayes.Feature(positive_class, negative_class, test_seq)
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
The class DNAStringSet can be obtained by using the function \emph{readDNAStringSet} avialble in \strong{Biostrings} package of Bioconductor. Here, the sequences must be supplied in FASTA format.
}
\value{
A numeric matrix of order \eqn{m*2n}, where \eqn{m} is the number of sequences in \code{test_seq} and \eqn{n} is the sequence length.

}
\references{
Zhang, Y., Chu, C., Chen, Y., Zha, H. and Ji, X. (2006). Splice site prediction using support vector machines with a Bayes kernel. \emph{Expert Systems with Applications}, 30: 73-81.
}
\author{
Prabina Kumar Meher, Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}

\seealso{
\code{\link{POS.Feature}}, \code{\link{MN.Fdtf.Feature}}
}
\examples{
data(droso)
positive <- droso$positive
negative <- droso$negative
test <- droso$test
pos <- positive[1:200]
neg <- negative[1:200]
tst <- test
enc <- Bayes.Feature(positive_class=pos, negative_class=neg, test_seq=tst)
enc
}

\keyword{Position weight matrix}
\keyword{Positional independence}
