\name{rankList}
\alias{rankList}

\title{
Find Rank List
}
\description{
\code{rankList} finds the rank of test list in the mutual information matrix of control to see any rank difference using mutual information matrix indexes.
}
\usage{
rankList(topTest, mimCont)
}

\arguments{
  \item{topTest}{A data matrix that has columns 4 and 5 have mim indexes}
  \item{mimCont}{A mutual information matrix of control dataset}
}
\details{
\code{rankList} takes mim indexes from 4th and 5th columns of the data matrix and finds the rank of test list in the mutual information matrix of control to see any rank difference.
}
\value{
  \code{rankList} returns a data matrix that has ranks of test list in the control MI matrix
}
\references{
  Altay, G., Asim, M., Markowetz, F., Neal, D.E. (2011) Differential C3NET reveals disease netwokrs of direct physical interactions. BMC Bioinformatics. 12: 296.
}
\author{
  Gokmen Altay
}

\seealso{
\code{\link{dc3net}}, \code{\link{maxofMIM}},
}

\keyword{misc}
