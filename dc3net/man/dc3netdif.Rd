\name{dc3netdif}
\alias{dc3netdif}

\title{
Find Differential Network
}
\description{
\code{dc3netdif} finds differential network list.
}
\usage{
dc3netdif(netTemp, vals, rankdif, percent)
}

\arguments{
  \item{netTemp}{Test network, e.g. tumor network.}
  \item{vals}{Statistics in the other (control) condition.}
  \item{rankdif}{Predefined rank cut-off parameter.}
  \item{percent}{Predefined rate parameter.}
}
\details{
\code{dc3netdif} finds differential network list. 
}
\value{
  \code{dc3netdif} returns differential network data matrix. The data matrix includes differential network list with gene names, mim indexes, probe names, control indexes, control mutual information rates and control ranks.
}
\references{
Altay, G. et. al. "Genome-wide differential gene network analysis software and an application in LnCap prostate cancer", bioRxiv, 2017. 
Altay, G., Asim, M., Markowetz, F., Neal, D.E. (2011) Differential C3NET reveals disease netwokrs of direct physical interactions. BMC Bioinformatics. 12: 296.
}
\author{
  Gokmen Altay
}

\seealso{
\code{\link{dc3net}}
}

\keyword{misc}
