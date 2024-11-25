\name{SASmodels}
\alias{SASmodels}
\title{
Information on available SAS models in the harmonization service
}
\description{
This information function shows the list of EC and pH harmonization models contained in the SAS harmonization service
}
\usage{
SASmodels(data="ec", extract="1:1")
}
\arguments{
  \item{data}{
either ec or ph data category for SAS harmonization models
}
\item{extract}{
extract solution for measuring ec or ph.
}
 }
\details{
This information function shows available models in the SAS harmonization service. The models are divided into two major categories: ec and ph models. The function returns a list of available SAS harmonization models under each data category. ec is the default data category.
}
\value{
A list of SAS harmonization models
}

\author{
Christian Thine Omuto
}
\seealso{
\code{\link{SASglobeData}}, \code{\link{DataAvailabilityIndex}}
}
\examples{
SASmodels("ec", "1:1")
SASmodels("ph","kcl")
}
