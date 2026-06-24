\name{dc3netcom}
\alias{dc3netcom}

\title{
Find Common Network
}
\description{
\code{dc3netcom} finds common network list.
}
\usage{
dc3netcom(netTemp, vals, rankdif, percent)
}

\arguments{
  \item{netTemp}{Test network, e.g. tumor network.}
  \item{vals}{Statistics in the other (control) condition.}
  \item{rankdif}{Predefined rank cut-off parameter.}
  \item{percent}{Predefined rate parameter.}
}
\details{
\code{dc3netcom} finds differential network list. 
}
\value{
  \code{dc3dif} returns common network data matrix. The data matrix includes differential network list with gene names, mim indexes, probe names, control indexes, control mutual information rates and control ranks.
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
