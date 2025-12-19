\name{plotProfilesAcrossAllSamples}
\alias{plotProfilesAcrossAllSamples}
\title{
Plot Gene Expression Profiles Across All Samples of the Original Data
}
\description{
Plot Gene Expression Profiles Across All Samples of the Original Data
}
\usage{
plotProfilesAcrossAllSamples(x, coreBiclusterGenes, coreBiclusterSamples)
}

\arguments{
  \item{x}{
data
}
  \item{coreBiclusterGenes}{
vector of genes belonging to bicluster
}
  \item{coreBiclusterSamples}{
vector of samples belonging to bicluster
}
}
\details{
The plot re-sorts the samples by bicluster membership and highlights them in red. Only the genes of a bicluster are plotted.
}
\value{
 no return value; a plot is drawn to the current device
}
\author{
Tatsiana Khamiakova
}
\seealso{
\code{\linkS4class{BiclustSet}} ,\code{\link{plotSuperAll}}
}