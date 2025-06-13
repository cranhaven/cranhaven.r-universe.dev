\name{MM1.Feature}
\alias{MM1.Feature}

\title{
Transforming nucleotide sequences into numeric vectors using first order nucleotide dependency.
}
\description{
The concept of sequence encoding using Markov model (\eqn{1^{st}} order) was introduced by Ho and Rajapakse (2005) for prediction of splice sites. However, this encoding scheme has been comprehensively used by Baten \emph{et al}. (2006) for prediction of splice sites. In this encoding procedure, first order dependencies between nucleotides in nucleotide sequence are accounted. Only the postive class dataset is used for estimation of dependencies in terms of probabilities, which are then used for encoding. 
}
\usage{
MM1.Feature(positive_class, test_seq)
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
The FASTA sequences should be read into R using the function \emph{readDNAStringSet} available in \strong{Biostrings} package. This encoding is similar to PN.FDTF feature, as far as the dependency among nucleotides in a sequence is concerned. The only difference is the use of positive class only in stead of both positive and negative classes in PN.FDTF. This encoding approach has similarity with WAM features (Meher \emph{et al}. 2016) in which the dinucleotide dependencies are considered.
}
\value{
A numeric matrix of order \eqn{m*(n-1)}, where \eqn{m} is the number of sequences in \code{test_seq} and \eqn{n} is the length of sequence.
}

\references{
\enumerate{
\item {Rajapakse, J. and Ho, L.S. (2005). Markov encoding for detecting signals in genomic sequences. \emph{IEEE/ACM Trans Comput Biol Bioinf}., 2(2): 131-142.}
\item {Baten, A., Chang, B., Halgamuge, S. and Li, J. (2006) Splice site identification using probabilistic parameters and SVM classification. \emph{BMC Bioinformatics}, 7(Suppl 5): S15.}
\item{Meher, P.K., Sahu, T.K., Rao, A.R. and Wahi, S.D. (2016). Identification of donor splice sites using support vector machine: a computational approach based on positional, compositional and dependency features. \emph{Algorithms for Molecular Biology}, 11(1), 16.}
}
}

\author{
Prabina Kumar Meher, Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}

\seealso{
\code{\link{PN.Fdtf.Feature}}, \code{\link{WAM.Feature}}
}

\examples{
data(droso)
positive <- droso$positive
test <- droso$test
pos <- positive[1:200]
tst <- test
enc <- MM1.Feature(positive_class=pos, test_seq=tst)
enc
}

\keyword{ Nucleotide dependency }
\keyword{ First order Markov model }
