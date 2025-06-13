\name{Maldoss.Feature}
\alias{Maldoss.Feature}
\title{
Encoding of nucleic acid sequences using di-nucleotide frequency difference between positive and negative class datasets. 
}
\description{
In Maldoss (Meher \emph{et al}., 2016), the authors propose three encoding approaches namely P1, P2 and P3. Out of these three encoding schemes, the accuracies were reported to be higher for P1 as compared to the other two encoding procedures. Here, we describe the sequence encodng based on P1 only. This P1 encoding approach has similarity with that of PN-FDTF encoding (Huang \emph{et al}., 2006) approach. The difference is only with respect to the logarithmic transformation in case of \code{Maldoss.Feature}. In this encoding procedure, both positive and negative class sequences are required for transformation of nucleotide sequences into numeric vectors.
}
\usage{
Maldoss.Feature(positive_class, negative_class, test_seq)
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
For getting an object of class \code{DNAStringSet}, the FASTA sequence dataset must be read in R through the function \emph{raedDNAStringSet} available in \strong{Biostrings} package of Bioconductor (\url{https://bioconductor.org/packages/release/bioc/html/Biostrings.html} ).    
}
\value{
A numeric matrix of order \eqn{m*(n-1)}, where \eqn{m} is the number of sequences in \code{test_seq} and \eqn{n} is the length of sequence.
}
\references{
\enumerate{
 \item{Meher, P.K., Sahu, T.K. and  Rao, A.R. (2016). Prediction of donor splice sites using random forest with a new sequence encoding approach. \emph{BioData Mining}, 9.}
 \item{Huang, J., Li, T., Chen, K. and Wu, J. (2006). An approach of encoding for prediction of splice sites using SVM. \emph{Biochimie}, 88(7): 923-929.}
}
}
\author{
Prabina Kumar Meher, Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}

\seealso{
\code{\link{PN.Fdtf.Feature}}, \code{\link{MM1.Feature}}, \code{\link{WAM.Feature}} 
}
\examples{
data(droso)
positive <- droso$positive
negative <- droso$negative
test <- droso$test
pos <- positive[1:200]
neg <- negative[1:200]
tst <- test
enc <- Maldoss.Feature(positive_class=pos, negative_class=neg, test_seq=tst)
enc
}

\keyword{Dinucleotide frequency }
\keyword{ True and False splice sites }
\keyword{Sequence encoding}
