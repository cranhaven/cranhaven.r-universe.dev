\name{MN.Fdtf.Feature}
\alias{MN.Fdtf.Feature}

\title{
Sequence encoding with nucleotide frequency difference between two classes of sequence datasets.
}
\description{
In this encoding procedure, at first, frequency of each nucleotide at each position is computed for both positive and negative classes datasets. Then, the frequency matrix of the positive set is substracted from that of negative set. The sequences are then encoded into numeric vectors after passing them through this difference matrix. So, both positive and negative datasets are necessary for encoding of sequences. This concept was introduced by Huang \emph{et al}. (2006), and was also used by Pashaei \emph{et al}. (2016) to generate features for prediction of splice sites along with other features. This has similarity with Bayes kernel encoding (Zhang \emph{et al}., 2006), where both frequency matrices are used for encoding instead of the difference matrix.
}
\usage{
MN.Fdtf.Feature(positive_class, negative_class, test_seq)
}

\arguments{
  \item{positive_class}{
Sequence dataset of the positive class, must be an object of class \code{\link{DNAStringSet}}.
}
  \item{negative_class}{
Sequence dataset of the negative class, must be an object of class \code{\link{DNAStringSet}}.
}
  \item{test_seq}{
Sequences to be encoded into numeric vectors, must be an object of class \code{\link{DNAStringSet}}.
}
}
\details{
For getting an object of class \code{DNAStringSet}, the sequence dataset must be read in FASTA format through the function \emph{readDNAStringSet} available in the \strong{Biostrings} package of Bioconductor (\url{https://bioconductor.org/packages/release/bioc/html/Biostrings.html} ).
}
\value{
A numeric matrix of order \eqn{m*n}, where \eqn{m} is the number of sequences in \code{test_seq} and \eqn{n} is the sequence length.
}
\references{
\enumerate{
\item {Zhang, Y., Chu, C., Chen, Y., Zha, H. and Ji, X. (2006). Splice site prediction using support vector machines with a Bayes kernel. \emph{Expert Systems with Applications}, 30: 73-81.}
\item{Huang, J., Li, T., Chen, K. and Wu, J. (2006). An approach of encoding for prediction of splice sites using SVM. \emph{Biochimie}, 88(7): 923-929.}
\item{Pashaei, E., Yilmaz, A., Ozen, M. and Aydin, N. (2016). Prediction of splice site using AdaBoost with a new sequence encoding approach. \emph{In Systems, Man, and Cybernetics (SMC), IEEE International Conference}, pp 3853-3858.}
}
}
\author{
Prabina Kumar Meher, Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}
\note{
This feature does not take into consideration the dependencies among nucleotides in the sequence.
}


\seealso{
 \code{\link{WMM.Feature}}, \code{\link{Bayes.Feature}}, \code{\link{PN.Fdtf.Feature}}
}
\examples{
data(droso)
positive <- droso$positive
negative <- droso$negative
test <- droso$test
pos <- positive[1:200]
neg <- negative[1:200]
tst <- test
enc <- MN.Fdtf.Feature(positive_class=pos, negative_class=neg, test_seq=tst)
enc
}

\keyword{ Position weight matrix }
\keyword{ Nucleotide frequency }
