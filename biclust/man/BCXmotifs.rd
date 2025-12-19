\name{BCXmotifs}
\title{The Xmotifs Bicluster algorithm}
\alias{BCXmotifs}
\alias{BCXmotifs-class}
\alias{Xmotif}
\alias{biclust,matrix,BCXmotifs-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\description{Performs XMotifs Biclustering based on the framework by Murali and Kasif (2003). Searches for a submatrix where each row as a similar motif through all columns.
  The Algorihm needs a discret matrix to perform.}
\usage{

\S4method{biclust}{matrix,BCXmotifs}(x, method=BCXmotifs(), ns=10, nd=10, sd=5, alpha=0.05, number=100)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Data Matrix.}
  \item{method}{Here BCXmotifs, to perform Xmotifs algorithm}
  \item{ns}{Number of columns choosen.}
  \item{nd}{Number of repetitions.}
  \item{sd}{Sample size in repetitions.}
  \item{alpha}{Scaling factor for column result.}
  \item{number}{Number of bicluster to be found.}
}

\value{
  Returns an object of class \code{Biclust}.
}

\section{Extends}{
Class \code{"\linkS4class{BiclustMethod}"}, directly.
}

\author{Sebastian Kaiser 
\email{sebastian.kaiser@stat.uni-muenchen.de}
}

\references{
Murali, T. & Kasif, S. 
Extracting Conserved Gene Expression Motifs from Gene Expression Data 
Pacific Symposium on Biocomputing, sullivan.bu.edu, 
2003, 8, 77-88


}

\seealso{ \code{\link{biclust}}, \code{\link{Biclust}}}
\examples{
data(BicatYeast)
x<-discretize(BicatYeast)
res <- biclust(x, method=BCXmotifs(), ns=20, nd=20, sd=5, alpha=0.01, number=10)
res
}

\keyword{cluster}
\keyword{classif}
