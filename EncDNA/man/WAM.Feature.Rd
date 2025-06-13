\name{WAM.Feature}
\alias{WAM.Feature}
\title{
Nucleic acid sequence encoding based on weighted array model.
}
\description{
Unlike weighted matrix method (WMM), first order nucleotide dependencies are accounted in weighted array model (WAM). The WAM was introduced by Zhang and Marr (1993) for locating splicing signal on nuclotide sequences. The WAM was employed by Meher \emph{et al}. (2016) for encoding of splice site motifs.
}
\usage{
WAM.Feature(positive_class, negative_class, test_seq)
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
In this encoding approach, a vector of two observations will be obtained for each sequence, corresponds to the situation when only positive class and both positive & neagtive datasets are used for encoding. This encoding scheme is also invariant to the length of the sequence.
}
\value{
A numeric matrix of order \eqn{m*2}, where \eqn{m} is the number of sequences in \code{test_seq}. 
}
\references{
\enumerate{
\item{Zhang, M. and Marr, T. (1993). A weight array method for splicing signal analysis. \emph{Comput Appl Biosci}., 9(5): 499-509.}
\item{Meher, P.K., Sahu, T.K., Rao, A.R. and Wahi, S.D. (2016). Identification of donor splice sites using support vector machine: a computational approach based on positional, compositional and dependency features. \emph{Algorithms for Molecular Biology}, 11(1): 16.} 
}
}
\author{
Prabina Kumar Meher, Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}

\seealso{
\code{\link{MM1.Feature}}, \code{\link{PN.Fdtf.Feature}}
}
\examples{
data(droso)
positive <- droso$positive
negative <- droso$negative
test <- droso$test
pos <- positive[1:200]
neg <- negative[1:200]
tst <- test
enc <- WAM.Feature(positive_class=pos, negative_class=neg, test_seq=tst)
enc
}

\keyword{Dinucleotide dependency }

