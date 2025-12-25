\name{groupcount1D}
\alias{groupcount1D}
\title{
Generate histogram count for an integer-valued vector.
}
\description{
Generate histogram count for an integer-valued vector.
}
\usage{
groupcount1D(g, n)
}
\arguments{
  \item{g}{
  An integer-valued input vector. The max value in g is n.
}
  \item{n}{
  The max value in g.
}
}
\details{
This is implemented as an utility function for 1D histgram count. For efficiency, it does not check if the maximum value in the input vector exceeds the maximum value specified.
}
\value{
The count values.
}

\author{
Quanli Wang
}
\examples{
n <- 20
g <- sample.int(n,1000, replace = TRUE)
counts <- groupcount1D(g,n)
}
