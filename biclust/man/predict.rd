\name{predictBimax}
\title{Predict from a BCrepBimax Result}
\alias{predictBimax}


\description{Predicts cluster membership for new data rows given a BCrepBimax Result}

\usage{
predictBimax(BCrepBimax, x)
}

\arguments{
  \item{BCrepBimax}{Result of biclust function with method BCrepBimax}
  \item{x}{The data matrix which clustermembership should be predicted}
  }
  
  
\value{
  Returns a vector with clustermembership of data x of class.
}
\author{Sebastian Kaiser 
\email{sebastian.kaiser@stat.uni-muenchen.de}
}


\seealso{ \code{\link{BCrepBimax}}}


\keyword{cluster}
\keyword{classif}
