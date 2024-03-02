\name{MittnikReducedModels}
\alias{MittnikReducedModels}
\title{Reduced Models via Mittnik SVD balancing}
\description{Reduced Models via Mittnik SVD balancing.}
\usage{
    MittnikReducedModels(largeModel)
}
\arguments{
    \item{largeModel}{An SS TSmodel.}
}
\value{A list of state space TSmodels with smaller state dimensions.}
\details{
The largeModel is balanced by the SVD method promoted by Mittnik (see 
MittnikReduction) and then models for every state dimension up to the 
state dimension of the largeModel are return. Note that this procedure 
does not result in smaller models which are balanced.
}
\seealso{
\code{\link{MittnikReduction}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
    z <-  MittnikReducedModels(toSS(estVARXls(eg1.DSE.data.diff))) 
}
\concept{DSE}
\keyword{ts}

