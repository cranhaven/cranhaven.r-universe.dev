\name{rfProximity}
\alias{rfProximity}
\title{ A random forest based proximity function }
\description{
Random forest computes similarity between instances with classification of out-of-bag instances. 
If two out-of-bag cases are classified in the same tree leaf the proximity between them is incremented.
}
\usage{
rfProximity(model, outProximity=TRUE)
}
\arguments{
  \item{model}{a \code{CORElearn} model of type random forest.}
  \item{outProximity}{if \code{TRUE}, function returns a proximity matrix, else it returns a distance matrix.}
}
\details{
A proximity is transformed into distance with expression \code{distance=sqrt(1-proximity)}.
}
\value{Function returns an M by M matrix where M is the number of training instances.
Returned matrix is used as an input to other function (see \code{\link{rfOutliers}}
and \code{\link{rfClustering}}).
}
\examples{
md <- CoreModel(Species ~ ., iris, model="rf", rfNoTrees=30, maxThreads=1)
pr <- rfProximity(md, outProximity=TRUE)
# visualization
require(lattice)
levelplot(pr)

destroyModels(md) # clean up

}

\author{ John Adeyanju Alao (as a part of his BSc thesis) and Marko Robnik-Sikonja (thesis supervisor)}

\seealso{
\code{\link{CoreModel}},
\code{\link{rfOutliers}},
\code{\link{cmdscale}},
\code{\link{rfClustering}}.
}
\references{
Leo Breiman: Random Forests. \emph{Machine Learning Journal}, 45:5-32, 2001
}

\keyword{cluster}
\keyword{robust}
