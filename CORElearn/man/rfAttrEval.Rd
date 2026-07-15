\name{rfAttrEval}
\alias{rfAttrEval}
\alias{rfAttrEvalClustering}
\title{ Attribute evaluation with random forest }
\description{
  The method evaluates the quality of the features/attributes/dependent variables
  used in the given random forest \code{model}.
  
}
\usage{
rfAttrEval(model) 
rfAttrEvalClustering(model, dataset, clustering=NULL)  }

\arguments{
  \item{model}{ The model of type \code{rf} or \code{rfNear} as returned by \code{\link{CoreModel}}. }
  \item{dataset}{ Training instances that produced random forest \code{model}.}
  \item{clustering}{ A clustering vector of \code{dataset} training instances used in \code{model}.}   
  }
\details{
  The attributes are evaluated via provided random forest's out-of-bag sets. Values for each attribute in turn
  are randomly shuffled and classified with random forest. The difference between average margin of
  non-shuffled and shuffled instances serves as a quality estimate of the attribute.
  The function \code{rfAttrEvalClustering} uses a clustering of the training instances to produce 
  importance score of attributes 
  for each cluster separately. If parameter \code{clustering} is set to \code{NULL} 
  the actual class values of the instances are used as clusters thereby  producing the evaluation of attributes 
  specific for each of the class values.  
}
\value{
  In case of \code{rfAttrEval} a vector of evaluations for the features in the order specified by the formula used to generate the provided \code{model}.
  In case of \code{rfAttrEvalClustering} a matrix is returned, where each row contains evaluations for one of the clusters. 
}

\author{ Marko Robnik-Sikonja (thesis supervisor) and John Adeyanju Alao (as a part of his BSc thesis)}


\keyword{ models }
\keyword{ nonlinear }
\keyword{ tree }
\keyword{ classif }

\seealso{
\code{\link{CORElearn}},
\code{\link{CoreModel}},
\code{\link{attrEval}}.
}

\examples{
# build random forests model with certain parameters
modelRF <- CoreModel(Species ~ ., iris, model="rf", 
              selectionEstimator="MDL", minNodeWeightRF=5, 
              rfNoTrees=100, maxThreads=1)
rfAttrEval(modelRF) # feature evaluations

x <- rfAttrEval(modelRF) # feature evaluations for each class
print(x)

destroyModels(modelRF) # clean up


}
\references{ 
Marko Robnik-Sikonja: Improving Random Forests. In J.-F. Boulicaut et al.(Eds): ECML 2004, 
LNAI 3210, Springer, Berlin, 2004, pp. 359-370
Available also from \url{http://lkm.fri.uni-lj.si/rmarko/papers/}
   
Leo Breiman: Random Forests. Machine Learning Journal, 2001, 45, 5-32

 }
