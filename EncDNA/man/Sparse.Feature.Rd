\name{Sparse.Feature}
\alias{Sparse.Feature}

\title{
Nucleotide sequence encoding with 0 and 1.
}
\description{
In this encoding approach A, T, G and C are encoded as (1,1,1), (1,0,0), (0,1,0) and (0,0,1). This was introduced by Golam Bari \emph{et al}. (2014). Besides, each nucleotide can also be encoded with four bits i.e., A as (1,0,0,0), T as (0,1,0,0), G as (0,0,1,0) and C as (0,0,0,1) as followed in Meher \emph{et al}. (2016).
}
\usage{
Sparse.Feature(test_seq)
}

\arguments{
  \item{test_seq}{
Sequence dataset to be encoded into numeric vector containing 0 and 1, must be an object of class \code{\link{DNAStringSet}}.
}
}
\details{
Each sequence is encoded independently, without the need of positive and negative classes datasets. 
}
\value{
A vector of length \eqn{4*n} for sequence of \eqn{n} nucleotides long in \code{test_seq}.
}
\references{
\enumerate{
\item{Bari, A.T.M.G., Reaz, M.R. and Jeong, B.S. (2014). Effective DNA encoding for splice site prediction using SVM. \emph{MATCH Commun. Math. Comput. Chem}., 71: 241-258.}
\item{Meher, P.K., Sahu, T.K., Rao, A.R. and Wahi, S.D. (2016). A computational approach for prediction of donor splice sites with improved accuracy. \emph{Journal of Theoretical Biology}, 404: 285-294.}
}
}
\author{
Prabina Kumar Meher, Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}
\note{
For larger sequence length, high dimensional feature vector will be generated.
}

\examples{
data(droso)
test <- droso$test
tst <- test
enc <- Sparse.Feature(test_seq=tst)
enc
}

\keyword{Binary encoding }

