\name{rfOutliers}
\alias{rfOutliers}
\title{ Random forest based outlier detection }
\description{
Based on random forest instance proximity measure detects training cases which are different to all other cases.
}
\usage{
rfOutliers(model, dataset)
}
\arguments{
  \item{model}{a random forest model returned by \code{\link{CoreModel}}}
  \item{dataset}{a training set used to generate the \code{model}}
}
\value{For each instance from a \code{dataset} the function returns a numeric score of its strangeness to other cases.}
\details{ 
Strangeness is defined using the random forest model via a proximity matrix (see \code{\link{rfProximity}}).
If the number is greater than 10, the case can be considered an outlier according to Breiman 2001.
}
\examples{
#first create a random forest tree using CORElearn
dataset <- iris
md <- CoreModel(Species ~ ., dataset, model="rf", rfNoTrees=30, 
                maxThreads=1)
outliers <- rfOutliers(md, dataset)
plot(abs(outliers))
#for a nicer display try 
plot(md, dataset, rfGraphType="outliers")

destroyModels(md) # clean up

}

\author{ John Adeyanju Alao (as a part of his BSc thesis) and Marko Robnik-Sikonja (thesis supervisor)}



\seealso{
\code{\link{CoreModel}},
\code{\link{rfProximity}},
\code{\link{rfClustering}}.
}
\references{
Leo Breiman: Random Forests. \emph{Machine Learning Journal}, 45:5-32, 2001
}

\keyword{cluster}
\keyword{robust}
