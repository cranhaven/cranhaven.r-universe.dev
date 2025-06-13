\name{SAE.Feature}
\alias{SAE.Feature}
\title{
Encoding of nucleotide sequences based on sum of absolute error (SAE) of each sequence.
}
\description{
The sum of absolute error (SAE) concept was introduced by Meher \emph{et al}. (2014) for prediction of donor splice sites, and was subsequently used by the same authors (Meher \emph{et al}., 2016) for encoding of splice site motif for prediction using supervised learning model. In this encoding technique also all possible pair-wise nucleotide dependencies are considered.
}
\usage{
SAE.Feature(positive_class, negative_class, test_seq)
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
In this encoding approach a vector of two observations will be obtained for each sequence. This two values correspond to the values obtained, when only positive class and both positive & neagtive datasets are used for encoding. This encoding scheme is invariant to the length of the sequence. Thus, both positive and negative classes datasets are required for encoding of sequence.
}
\value{
A numeric matrix of order \eqn{m*2}, where \eqn{m} is the number of sequences in \code{test_seq}. 
}
\references{
\enumerate{
\item{Meher, P.K., Sahu, T.K., Rao, A.R. and Wahi, S.D. (2014). A statistical approach for 5' splice site prediction using short sequence motifs and without encoding sequence data. \emph{BMC Bioinformatics}, 15(1), 362.}

\item{Meher, P.K., Sahu, T.K., Rao, A.R. and Wahi, S.D. (2016). Identification of donor splice sites using support vector machine: a computational approach based on positional, compositional and dependency features. \emph{Algorithms for Molecular Biology}, 11(1), 16.}
}
}
\author{
Prabina Kumar Meher, Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}

\seealso{
\code{\link{MM1.Feature}}, \code{\link{WAM.Feature}}
}
\examples{
data(droso)
positive <- droso$positive
negative <- droso$negative
test <- droso$test
pos <- positive[1:200]
neg <- negative[1:200]
tst <- test
enc <- SAE.Feature(positive_class=pos, negative_class=neg, test_seq=tst)
enc
}
\keyword{All possible pair-wise dependency}
\keyword{ Splice sites motifs }
