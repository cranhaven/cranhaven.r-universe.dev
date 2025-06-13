\name{POS.Feature}
\alias{POS.Feature}

\title{
Transformation of nucleic acid sequences into numeric vectors using position-wise frequency of nucleotides.
}
\description{
This encoding scheme was devised by Li \emph{et al}. (2012). Frequencies of 4 nucleotides are first computed at each position for both positive and negative datasets, resulting in two \eqn{4*L} probability tables for the two classes for sequence length \eqn{L}. A \eqn{4*L} statistical difference table is obtained by elementwise substraction of the two probability distribution tables, which is then used for encoding of sequences. Further, as per sparse encoding, the nucleotides A, T, G and C can be encoded as (1,0,0,0), (0,1,0,0), (0,0,1,0) and (0,0,0,1) respectively. The value 1 of sparse encoding is then replaced with the difference values obtained from the difference table for encoding nucleotide at each postion. Thus, it can be said that POS feature encoding is a blending of MN-FDTF (Huang \emph{et al}., 2006) and Sparse encoding (Meher \emph{et al}., 2016) technique. 
}
\usage{
POS.Feature(positive_class, negative_class, test_seq)
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
The \code{DNAstringSet} object can be obtained by reading the sequences in FASTA format using the function \emph{readDNAStringSet}available in the \strong{Biostrings} package of Bioconductor.
}
\value{
A numeric matrix of order \eqn{m*4n}, where \eqn{m} is the number of sequences in \code{test_seq} and \eqn{n} is the length of sequence.
}
\references{
\enumerate{
\item{Huang, J., Li, T., Chen, K. and Wu, J. (2006). An approach of encoding for prediction of splice sites using SVM. \emph{Biochimie}, 88(7): 923-929.}
\item{Li, J.L., Wang, L.F., Wang, H.Y., Bai, L.Y., Yuan, Z.M. (2012). High-accuracy splice sites prediction based on sequence component and position features. \emph{Genetics and Molecular Research}, 11(3): 3432-3451.}
\item{Meher, P.K., Sahu, T.K., Rao, A.R. and Wahi, S.D. (2016). A computational approach for prediction of donor splice sites with improved accuracy. \emph{Journal of Theoretical Biology}, 404: 285-294.}
}
}
\author{
Prabina Kumar Meher, Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}
\note{
In this encoding procedure, dependencies of nucleotides are not taken into consideration. Both positive and negative datasets are required for encoding of nucleotide sequences. Each sequence of length \eqn{L} can be transformed into a numeric vector of length \eqn{4*L} with this encoding technique.
}

\seealso{
 \code{\link{MN.Fdtf.Feature}}, \code{\link{Bayes.Feature}}, \code{\link{WMM.Feature}}
}
\examples{
data(droso)
positive <- droso$positive
negative <- droso$negative
test <- droso$test
pos <- positive[1:200]
neg <- negative[1:200]
tst <- test
enc <- POS.Feature(positive_class=pos, negative_class=neg, test_seq=tst)
enc
}

\keyword{ Position weight matrix }
\keyword{ Nucleotide frequency }
