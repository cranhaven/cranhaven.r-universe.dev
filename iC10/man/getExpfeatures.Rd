\name{getExpfeatures}
\alias{getExpfeatures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internal function for matching expression features.
}
\description{
Internal function for matching expression features.
}
\usage{
getExpfeatures(Exp, Probes, Synonyms, by.feat)
}
\arguments{
  \item{Exp}{
  Matrix of expression features
}
  \item{Probes}{
  Vector of probes to match
}
  \item{Synonyms}{
vector of synonyms fo gene names
}
  \item{by.feat}{
either "probe" or "gene"
}
}
\value{
A matrix with the \code{Probes} in Exp.
}
\references{
Curtis et al. The genomic and transcriptomic architecture of 2,000
breast tumours reveals novel subgroups. Nature 2012; 486:346-352.
}
\author{
Oscar M Rueda
}
\note{
This function is not supposed to be called directly. use \code{matchFeatures} instead.
}

\seealso{
\code{\link{matchFeatures}}
}
