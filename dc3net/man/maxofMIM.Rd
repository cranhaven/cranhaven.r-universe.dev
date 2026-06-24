\name{maxofMIM}
\alias{maxofMIM}

\title{
Find Max Partner For Each Gene
}
\description{
\code{maxofMIM} takes the mutual information matrix as input and find max partner gene with highest mutual information value for each gene.
}
\usage{
maxofMIM(mim)
}

\arguments{
  \item{mim}{A symmetric square mutual information matrix, where the elements (i,j) correspond to the mutual information \eqn{I(i,j)} or \eqn{I(j,i)} between variables i and j.
}
}
\details{
  \code{maxofMIM} scans each row of the mutual information matrix and find the column with the highest mutual information value for each row. Thus, it finds max partner gene with highest mutual information value for each gene.
}
\value{
  \code{maxofMIM} returns a data matrix that rows are probe/gene names and columns are mutual information values, row numbers of each gene and row numbers of partner gene with highest mutual information value, respectively.
}
\references{
Altay, G. et. al. "Genome-wide differential gene network analysis software and an application in LnCap prostate cancer", bioRxiv, 2017. 
Altay, G., Asim, M., Markowetz, F., Neal, D.E. (2011) Differential C3NET reveals disease netwokrs of direct physical interactions. BMC Bioinformatics. 12: 296.
}
\author{
  Gokmen Altay
}

\seealso{
  \code{\link{dc3net}}, \code{\link{orderList}},
}

\keyword{misc}
