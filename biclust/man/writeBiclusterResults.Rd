\name{writeBiclusterResults}
\alias{writeBiclusterResults}
\alias{writeBiclusters}


\title{writeBiclusterResults}
\description{ Write bicluster results to a file}
\usage{
writeBiclusterResults(fileName, bicResult, bicName, geneNames, arrayNames,
  append=FALSE, delimiter=" ")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fileName}{Path to the file were biclusters are written.}
  \item{bicResult}{Biclusters results as a Biclust class.}
  \item{bicName}{Brief description for the biclustering algorithm used.}
  \item{geneNames}{Array of strings with gene (row) names in the analyzed data matrix}
  \item{arrayNames}{Array of strings with condition (column) names in the analyzed data matrix}
  \item{append}{If true, adds the bicluster results to previous information in the text file, if it exists. Default false.}
  \item{delimiter}{delimiter string between gene and condition names. Default " ".}
  }

\author{
  Rodrigo Santamaria
  \email{rodri@usal.es}
  }

\examples{

\dontrun{
  data(BicatYeast)
  res <- biclust(BicatYeast, method=BCCC(), delta=1.5,  alpha=1, number=10)
  writeBiclusterResults("results.txt", res,"CC with delta 1.5", dimnames(BicatYeast)[1][[1]],
    dimnames(BicatYeast)[2][[1]])
  }
  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{array}
