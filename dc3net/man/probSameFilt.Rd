\name{probSameFilt}
\alias{probSameFilt}

\title{
Filter Probes of The Same Gene
}
\description{
\code{probSameFilt} takes the mutual information matrix as input and set the mutual information values of same genes to zero. Thus, it eliminates the interactions between the probes of the same gene.
}
\usage{
probSameFilt(mim, genes)
}

\arguments{
  \item{mim}{A symmetric square mutual information matrix, where the elements (i,j) correspond to the mutual information \eqn{I(i,j)} or \eqn{I(j,i)} between variables i and j.
}
  \item{genes}{
A vector of gene names
}
}
\details{
The use of \code{probSameFilt} is very important when working on probe level. If this function is not enabled, then the interactions between probes of the same gene, which should not be taken into account, will be included in differential or common network computation. This may result in difnet or commonnet tables which includes same gene interactions e.g. GPR50-GPR50.
}
\value{
  \code{probSameFilt} returns mutual information matrix that the interaction scores between probes of the same gene is updated to 0.
}
\references{
Altay, G. et. al. "Genome-wide differential gene network analysis software and an application in LnCap prostate cancer", bioRxiv, 2017. 
Altay, G., Asim, M., Markowetz, F., Neal, D.E. (2011) Differential C3NET reveals disease netwokrs of direct physical interactions. BMC Bioinformatics. 12: 296.
}
\author{
  Gokmen Altay
}

\seealso{
  \code{\link{dc3net}},
}
\examples{
  data(tumorData)

  data(geneNames)
  
  tumorData <- copula(tumorData) 
      
  tumorData <- makemim(tumorData)

  probSameFilt(tumorData, geneNames)

}
\keyword{misc}