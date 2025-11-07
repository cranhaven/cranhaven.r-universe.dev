\name{ari}
\alias{ari}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Adjusted Rand index}
\description{
The adjusted Rand index (ARI) allows to compare two clustering partitions.
}
\usage{
ari(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The first partition to compare (as vector).
}
  \item{y}{
The second partition to compare (as vector).
}
}
\value{
  \item{ari}{The value of the ARI.}
}

\seealso{
\code{\link{funLBM}}
}
\examples{
x = sample(1:3,20,replace = TRUE)
y = sample(1:3,20,replace = TRUE)
ari(x,y)
}