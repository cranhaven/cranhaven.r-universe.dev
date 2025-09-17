\name{getMembershipMatrix}
\alias{getMembershipMatrix}
\title{
A function that generates a data.frame that is the membership matrix of the network.
}
\description{
A function that generates a data.frame that is the membership matrix of the network given individual IDs and the alters that they have nominated.
}
\usage{
  getMembershipMatrix(individualID, alters)
}
\arguments{
  \item{individualID}{
  A data.frame which stores the IDs of the individuals that nominate alters.
  }
  \item{alters}{
  A data.frame which stores the alters of a given individual.
  }
}
\value{
  \item{membershipMatrix}{The resultant data.frame.}
}
\author{
George Gerogiannis
}
\examples{
  
  individualID = data.frame(c(1, 2, 3))
  alters = data.frame(c(5, 3, 2), c(5, 6, 1))
  getMembershipMatrix(individualID, alters)
  
  individualID = data.frame(c(1, 2, 3))
  alters = data.frame(c(NA, 3, 2), c(NA, NA, 1))
  getMembershipMatrix(individualID, alters)
  
  individualID = data.frame(c(1, 2, 3))
  alters = data.frame(c(NA, 3, NA), c(NA, NA, 1))
  getMembershipMatrix(individualID, alters)
  
  individualID = data.frame(c(1, 2, 3))
  alters = data.frame(c(NA, 3, NA), c(6, NA, 1))
  getMembershipMatrix(individualID, alters)
  
}
