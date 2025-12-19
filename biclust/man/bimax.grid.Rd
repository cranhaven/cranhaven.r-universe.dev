\name{bimax.grid}
\alias{bimax.grid}

\title{
Parameter Grid for BCBimax Biclustering
}
\description{
Generates a list containing parameter settings for the ensemble algorithm. 
}
\usage{
bimax.grid(method = "BCBimax", minr = c(10, 11), minc = c(10, 11), number = 10)
}

\arguments{
  \item{method}{Here BCBimax, to perform Bimax algorithm}
  \item{minr}{Minimum row size of resulting bicluster.}
  \item{minc}{Minimum column size of resulting bicluster.}
  \item{number}{Number of Bicluster to be found.}
}

\value{
A list containing parameter settings
}

\author{Sebastian Kaiser 
\email{sebastian.kaiser@stat.uni-muenchen.de}
}

\seealso{
\code{\link{ensemble}}, \code{\link{BCBimax}}
}
\examples{
bimax.grid()
}
\keyword{ cluster }
