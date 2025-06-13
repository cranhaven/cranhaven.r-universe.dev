\name{MM2.Feature}
\alias{MM2.Feature}

\title{
Mapping nucleotide sequences onto numeric feature vectors based on second order nucleotide dependencies.
}
\description{
This encoding procedure is similar to the MM1 encoding. The only difference is consideration of second order dependencies unlike first order in \code{MM2.Feature}. This technique was first conceptualized by Rajapakse and Ho (2005), and adopted by Maji and Garg (2014). The number of parameters to be estimated in MM2 is 64, which is higher than that of MM1 i.e., 16. Further, only the positive class dataset is used for encoding of sequences.
}
\usage{
MM2.Feature(positive_class, test_seq)
}

\arguments{
  \item{positive_class}{
Sequence dataset of the positive class, must be an object of class \code{\link{DNAStringSet}}.
}
  \item{test_seq}{
Sequences to be encoded into numeric vectors, must be an object of class \code{\link{DNAStringSet}}.
}
}
\details{
For getting an object of class \code{DNAStringSet}, the FASTA sequences should be read using the function \emph{readDNAStringSet} avialble in the \strong{Biostrings} package.
}
\value{
A numeric matrix of order \eqn{m*(n-2)}, where \eqn{m} is the number of sequences in \code{test_seq} and \eqn{n} is the length of sequence.
}
\references{
\enumerate{
\item {Rajapakse, J. and Ho, L.S. (2005). Markov encoding for detecting signals in genomic sequences. \emph{IEEE/ACM Trans Comput Biol Bioinf}., 2(2): 131-142.}
\item {Maji, S. and Garg, D. (2014). Hybrid approach using SVM and MM2 in splice site junction identification. \emph{Current Bioinformatics}, 9(1): 76-85.}
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
test <- droso$test
pos <- positive[1:200]
tst <- test
enc <- MM2.Feature(positive_class=pos, test_seq=tst)
enc
}

\keyword{Second order Markov model}
\keyword{ Nucleotide dependency }
