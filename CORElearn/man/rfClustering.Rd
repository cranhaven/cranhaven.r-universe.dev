\name{rfClustering}
\alias{rfClustering}
\title{Random forest based clustering}
\description{
Creates a clustering of random forest training instances. Random forest provides proximity of its training instances based on their out-of-bag classification.
This information is usually passed to visualizations (e.g., scaling) and attribute importance measures.
}
\usage{
rfClustering(model, noClusters=4)
}
\arguments{
  \item{model}{ a random forest model returned by \code{\link{CoreModel}}}
  \item{noClusters}{number of clusters}
}
\details{
The method calls \code{\link[cluster]{pam}} function for clustering, initializing its distance matrix with random forest based similarity by calling
\code{\link{rfProximity}} with argument \code{model}.
}
\value{
 An object of class \code{pam} representing the clustering (see \code{?pam.object} for details),
 the most important being a vector of cluster assignments (named \code{cluster}) to training instances used to generate the \code{model}.
}

\examples{
set<-iris
md<-CoreModel(Species ~ ., set, model="rf", rfNoTrees=30, maxThreads=1)
mdCluster<-rfClustering(md, 5)

destroyModels(md) # clean up

}
\author{ John Adeyanju Alao (as a part of his BSc thesis) and Marko Robnik-Sikonja (thesis supervisor)}


\seealso{
\code{\link{CoreModel}}
\code{\link{rfProximity}}
\code{\link[cluster]{pam}}
}

\references{
Leo Breiman: Random Forests. \emph{Machine Learning Journal}, 45:5-32, 2001
}


\keyword{cluster}
\keyword{robust}
\keyword{tree}
