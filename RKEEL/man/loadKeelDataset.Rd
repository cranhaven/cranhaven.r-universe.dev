\docType{methods}
\name{loadKeelDataset}
\alias{loadKeelDataset}
\title{Load KEEL Dataset}
\description{
Loads a dataset of the KEEL datasets repository.
The included datasets names are available at the \code{getKeelDatasetList} method of RKEELdata.
}
\usage{
loadKeelDataset(dataName)
}
\arguments{
\item{dataName}{String with the correct data name of one of the KEEL datasets}
}
\value{
Returns a data.frame with the KEEL dataset.
}
\examples{
RKEEL::loadKeelDataset("iris")
}
\keyword{utils}

