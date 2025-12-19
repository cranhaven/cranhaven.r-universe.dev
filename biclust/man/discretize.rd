\name{discretize}
\alias{discretize}


\title{Create a discret matrix}
\description{ Some biclusteralgorithms need a discret matrix to perform well.
This function delivers a discret matrix with either a given number of levels of equally spaced intervals from minimum to maximum, or levels of same size using the quantiles.
}

\usage{
discretize(x,nof=10,quant=FALSE)
}

\arguments{
  \item{x}{The data matrix from which should be dicretized}
  \item{nof}{Number of levels}
  \item{quant}{If TRUE using the quantiles, else using equally spaced levels}
  }

\author{
  Sebastian Kaiser
  \email{sebastian.kaiser@stat.uni-muenchen.de}
  }

%\seealso{  }

\examples{
  #Discretize yeast microarray data
  data(BicatYeast)
  discretize(BicatYeast[1:10,1:10])

  }



\keyword{cluster}