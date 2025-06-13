\name{Trint.Dist.Feature}
\alias{Trint.Dist.Feature}

\title{
Tri-nucleotide distribution-based encoding of nucleotide sequences.
}
\description{
This encoding scheme was first time adopted by Wei \emph{et al}. (2013) for prediction of splice sites along with MM1 features. In this encoding technique, distribution of trinucleotides are taken into consideration independently for the exon and intron regions of splice site motifs.
}
\usage{
Trint.Dist.Feature(test_seq)
}

\arguments{
  \item{test_seq}{
Sequence dataset to be transformed into numeric feature vectors. There should be atleat two sequences, must be an object of class \code{\link{DNAStringSet}}.
}
}
\details{
This encoding scheme is independent of positive and negative datasets. In other words, each sequence can be encoded independently. Further, nucleotide sequence of any length will be transformed into a numeric vector of 64 observations corresponding to 64 combinations of trinucleotides.
}
\value{
A numeric matrix of order \eqn{m*64}, where \eqn{m} is the number of sequences in \code{test_seq}.
}
\references{
Wei, D., Zhang, H., Wei, Y. and Jiang, Q. (2013). A novel splice site prediction method using support vector machine. \emph{J Comput Inform Syst}., 920: 8053-8060.
}
\author{
Prabina Kumar Meher, Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}


\examples{
data(droso)
test <- droso$test
tst <- test
enc <- Trint.Dist.Feature(test_seq=tst)
enc
}

\keyword{ Tri-nucleotide frequency }

