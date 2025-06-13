\name{Predoss.Feature}
\alias{Predoss.Feature}

\title{
Encoding nucleotide sequences using all possible di-nucleotide dependencies.
}
\description{
In this encoding, not only the adjecent dependencies are considered, but also the association that exists among non-adjacent nucleotides. In MM1, PN.FDTF features, only the dependencies between adjacent nucleotides are taken into account. Though all possible pair-wise dependencies are first introduced by Meher \emph{et al}. (2014) for predicting splice sites through probablistic approach, the same authors further used this association to encode the splice site dataset for prediction using machine learning classifiers (Meher \emph{et al}., 2016).  
}
\usage{
Predoss.Feature(positive_class, negative_class, test_seq)
}

\arguments{
  \item{positive_class}{
Sequence dataset of the positive class, must be an object of class \code{\link{DNAStringSet}}.
}
  \item{negative_class}{
Sequence dataset of the negative class, must be an object of class \code{\link{DNAStringSet}}.
}
  \item{test_seq}{
Sequences to be encoded into numeric vectors, must be of an object of class \code{\link{DNAStringSet}}.
}
}
\details{
This encoding approach will be helpful for transformation of nucleotide sequences into numeric feature vectors, which can subsequently be used as input in several supervised learning models for classification.
}
\value{
A numeric matrix of order \eqn{m*n^{2}}, where \eqn{m} is the number of sequences in \code{test_seq} and \eqn{n} is the length of sequence.
}
\references{
\enumerate{
\item{Meher, P.K., Sahu, T.K., Rao, A.R. and Wahi, S.D. (2014). A statistical approach for 5' splice site prediction using short sequence motifs and without encoding sequence data. \emph{BMC Bioinformatics}, 15(1), 362.}

\item{Meher, P.K., Sahu, T.K., Rao, A.R. and Wahi, S.D. (2016). A computational approach for prediction of donor splice sites with improved accuracy. \emph{Journal of Theoretical Biology}, 404: 285-294.}
}
}
\author{
Prabina Kumar Meher, Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}
\note{
Dimension of the feature space will increase geometrically with increase in the length of the sequence.
}

\examples{
data(droso)
positive <- droso$positive
negative <- droso$negative
test <- droso$test
pos <- positive[1:200]
neg <- negative[1:200]
tst <- test
enc <- Predoss.Feature(positive_class=pos, negative_class=neg, test_seq=tst)
enc
}
\keyword{Nucleotide dependencies}
\keyword{Binary classification}
\keyword{Multiclass classification}
