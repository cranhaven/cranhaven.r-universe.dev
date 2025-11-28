\name{clusters.detection}
\alias{clusters.detection}
\title{
Detection of clusters.
}
\description{
This function calculates the structural similarity between different metabolites, performs hierarchical clustering using the KODAMA algorithm, and detects the optimal number of clusters. The procedure is repeated to ensure the robustness of the detection.
}
\usage{


clusters.detection  (smiles,
                     k=50,
                     seed=12345,
                     max_nc = 30,
                     dissimilarity.parameters=list(),
                     kodama.matrix.parameters=list(),
                     kodama.visualization.parameters=list(),
                     hclust.parameters=list(method="ward.D"),
                     verbose = TRUE)

}

\arguments{
\item{smiles}{
A list of smile notations for the study metabolites dataset.
}
\item{k}{
The number of components of multidimensional scaling.
}
\item{seed}{
Seed for the generation of random numbers.
}
\item{max_nc}{
Maximum number of clusters.
}
\item{dissimilarity.parameters}{
Optional parameters for  \code{\link{chemical.dissimilarity}} function.
}
\item{kodama.matrix.parameters}{
Optional parameters for  \code{\link[KODAMA]{KODAMA.matrix}} function.
}
\item{kodama.visualization.parameters}{
Optional parameters for  \code{\link[KODAMA]{KODAMA.visualization}} function.
}
\item{hclust.parameters}{
Optional parameters for  \code{\link{hclust}} function.
}
\item{verbose}{
If verbose is TRUE, it displays the progress for each iteration.
}
}

\value{
  A list contains all results of KODAMA chemical similarity analysis and hierarchical clustering.

}


\seealso{
  \code{\link[KODAMA]{KODAMA.matrix}}, \code{\link[KODAMA]{KODAMA.visualization}}
}

\examples{
\donttest{
data(Metabolites)

res=clusters.detection(Metabolites$SMILES) 

}
}

