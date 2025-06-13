\name{WMM.Feature}
\alias{WMM.Feature}

\title{ 
Weighted matrix model based mapping of nucleotide sequences into vectors of numeric observations.
}
\description{
The weighted matrix model (WMM) was developed by Staden (1984) for prediction of splice sites. In this technique, position weight matrix are computed from the aligned positive dataset which is then used for computing the liklihood of any nucleotide sequence being a positive or negative class. The position weight matrix was later on used by Meher et al. (2016) for encoding of splice site motifs for prediction using supervised learning models. 
}
\usage{
WMM.Feature(positive_class, negative_class, test_seq)
}

\arguments{
  \item{positive_class}{
Sequence dataset of the positive class, must be an object of class \code{\link{DNAStringSet}}.
}
  \item{negative_class}{
Sequence dataset of the negative class, must be an object of class \code{\link{DNAStringSet}}.
}
  \item{test_seq}{
Sequences to be encoded into numeric vector, must be an object of class \code{\link{DNAStringSet}}.
}
}
\details{
In this encoding approach, a vector of two observations will be obtained for each sequence, corresponds to the situation when only positive class and both positive and neagtive datasets are used for encoding. This encoding scheme is also invariant to the length of the sequence.
}
\value{
A numeric matrix of order \eqn{m*2}, where \eqn{m} is the number of sequences in \code{test_seq}. 
}
\references{
\enumerate{
\item{Staden, R. (1984). Computer methods to locate signals in nucleic acid sequences. \emph{Nucleic Acids Research}, 12: 505-519.}
\item{Meher, P.K., Sahu, T.K., Rao, A.R. and Wahi, S.D. (2016). Identification of donor splice sites using support vector machine: a computational approach based on positional, compositional and dependency features. \emph{Algorithms for Molecular Biology}, 11(1), 16.} 
}
}
\author{
Prabina Kumar Meher, Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}

\seealso{
\code{\link{Bayes.Feature}}, \code{\link{MN.Fdtf.Feature}}
}
\examples{
data(droso)
positive <- droso$positive
negative <- droso$negative
test <- droso$test
pos <- positive[1:200]
neg <- negative[1:200]
tst <- test
enc <- WMM.Feature(positive_class=pos, negative_class=neg, test_seq=tst)
enc
}

\keyword{Position weight matrix}
\keyword{Positional independence }
