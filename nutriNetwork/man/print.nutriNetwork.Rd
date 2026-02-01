\name{print.nutriNetwork}
\alias{print.nutriNetwork}
\title{
Print function for S3 class "nutriNetwork"
}
\description{
Print a summary of results from function \code{\link{nutriNetwork}}. 
}
\usage{
\method{print}{nutriNetwork}(x, ...)
}
\arguments{
  \item{x}{
An object with S3 class "nutriNetwork"
}
  \item{\dots}{
System reserved (No specific usage)
}
}

\author{ 
  Pariya Behrouzi and Ernst C. Wit \cr
  Maintainer: Pariya Behrouzi \email{pariya.behrouzi@gmail.com} \cr
}

\seealso{
\code{\link{nutriNetwork}}
}

\examples{
data(vfit)
out <- nutriNetwork(vfit, method ="npn"); out
}