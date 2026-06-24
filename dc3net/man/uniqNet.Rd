\name{uniqNet}
\alias{uniqNet}

\title{
Unique Network
}
\description{
\code{uniqNet} makes the differential or common network list unique.
}
\usage{
uniqNet(tops = c())
}

\arguments{
  \item{tops}{A data matrix that contains list of interactions.}
}
\details{
\code{uniqNet} removes the redundant interactions in the generated differential or common network. For ex. it is possible that the generated network can include both geneA-to-geneB and geneB-to-geneA interactions. \code{uniqNet} eliminates one of this interaction so the list of final network is unique.
}
\value{
  \code{uniqNet} returns a unique data matrix that includes network list with gene names, mim indexes, probe names, control indexes, control mutual information rates and control ranks.
}
\references{
Altay, G. et. al. "Genome-wide differential gene network analysis software and an application in LnCap prostate cancer", bioRxiv, 2017. 
Altay, G., Asim, M., Markowetz, F., Neal, D.E. (2011) Differential C3NET reveals disease netwokrs of direct physical interactions. BMC Bioinformatics. 12: 296.
}
\author{
  Gokmen Altay
}

\seealso{
\code{\link{dc3net}}, \code{\link{maxofMIM}}, \code{\link{orderList}}, \code{\link{rankList}}, \code{\link{dc3netdif}}, \code{\link{dc3netcom}}
}

\keyword{misc}
