\name{PN.Fdtf.Feature}
\alias{PN.Fdtf.Feature}
\title{
Conversion of nucleotide sequences into numeric feature vectors based on the difference of dinucleotide frequency.
}
\description{
Dinucleotide frequency matrix is first computed for both positive and negative classes. Then, frequency matrix of the positive class is substracted from that of negative class. The sequences are then passed through this difference matrix to encode them into numeric feature vectors. Similar to the \code{MN.Fdtf feature}, both positive and negative classes are necessary for encoding of nucleotide sequences. This was also conceptualized by Huang \emph{et al}. (2006). This has also been used by Pashaei \emph{et al}. (2016) as one of the features for prediction of splice sites along with the other features. 
}
\usage{
PN.Fdtf.Feature(positive_class, negative_class, test_seq)
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
For getting an object of class \code{DNAStringSet}, the sequence dataset must be read in FASTA format through the function \emph{readDNAStringSet} available in \strong{Biostrings} package of Bioconductor (\url{https://bioconductor.org/packages/release/bioc/html/Biostrings.html} ).
}
\value{
 A numeric matrix of order \eqn{m*(n-1)}, where \eqn{m} is the number of sequences in \code{test_seq} and \eqn{n} is the sequence length.
}
\references{
\enumerate{
\item{Huang, J., Li, T., Chen, K. and Wu, J. (2006). An approach of encoding for prediction of splice sites using SVM. \emph{Biochimie}, 88(7): 923-929.}
\item{Pashaei, E., Yilmaz, A., Ozen, M. and Aydin, N. (2016). Prediction of splice site using AdaBoost with a new sequence encoding approach. \emph{In Systems, Man, and Cybernetics (SMC), IEEE International Conference}, pp 3853-3858.}
}
}
\author{
Prabina Kumar Meher, Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}
\note{
Both positive and negative classes datasets are essential for the encoding. This feature has similarity with that of \code{MM1.Feature} and \code{WAM.Feature} with respect to the first order dependency. Unlike \code{MN.Fdtf.Feature}, this feature takes into account the first order dependencies of nucleotides in the sequence.
}

\seealso{
\code{\link{MN.Fdtf.Feature}}, \code{\link{WAM.Feature}}, \code{\link{MM1.Feature}},  
}
\examples{
data(droso)
positive <- droso$positive
negative <- droso$negative
test <- droso$test
pos <- positive[1:200]
neg <- negative[1:200]
tst <- test
enc <- PN.Fdtf.Feature(positive_class=pos, negative_class=neg, test_seq=tst)
enc
}

\keyword{Nucleotide dependencies }
\keyword{ Dinucleotide frequency }
\keyword{Splice sites}
