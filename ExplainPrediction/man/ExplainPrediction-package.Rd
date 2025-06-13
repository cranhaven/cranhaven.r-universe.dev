\name{ExplainPrediction-package}
\alias{ExplainPrediction-package}
\alias{ExplainPrediction}

\docType{package}
\title{Explanation of individual predictions and models}
\description{
The package ExplainPrediction contains methods to generate explanations for individual predictions of 
classification and regression models. 
}
\details{
 The explanation methodology used is based on measuring contributions of individual features on
an individual predictions. The contributions of all attributes present an explanation of individual prediction.
Explanations can be visualized with a nomogram. If we average the explanations, we get an explanation of the 
whole model. Two explanation methods are implemented:
 \itemize{
\item EXPLAIN (described in \emph{Explaining Classifications For Individual Instances}). The EXPLAIN method is much faster 
      than IME and works for any number of attributes in the model, but cannot explain dependencies expressed disjunctively
       in the model. For details see \code{\link{explainVis}}. 
\item IME can in principle explain any type of dependencies in the model. 
It uses sampling based method to avoid exhaustive search for dependencies and 
          works reasonably fast for up to a few dozen attributes in the model. The details see the references.
}

Currently prediction models implemented in package \link{CORElearn} are supported, 
for other models a wrapper of class \code{\link{CoreModel}} has to be created. 
The wrapper has to present the model with a list with the following components:
\itemize{
\item \code{formula} of class \code{\link{formula}} representing the response and predictive variables,
\item \code{noClasses} number of class values in class of classification model, 0 in case of regression,
\item \code{class.lev} the levels used in representation of class values (see \code{\link{factor}}),
}
Additionally it has to implement function \code{\link{predict}} which returns the same components as the function
 \code{\link{predict.CoreModel}} in the package \link{CORElearn}.

Further software and development versions of the package are available at \url{http://lkm.fri.uni-lj.si/rmarko/software}. 
}
\author{
Marko Robnik-Sikonja 
}
\references{
Marko Robnik-Sikonja, Igor Kononenko: Explaining Classifications For Individual Instances.
\emph{IEEE Transactions on Knowledge and Data Engineering}, 20:589-600, 2008

Erik Strumbelj, Igor Kononenko, Igor, Marko Robnik-Sikonja: Explaining Instance Classifications with Interactions of 
Subsets of Feature Values. \emph{Data and Knowledge Engineering}, 68(10):886-904, Oct. 2009

Erik Strumbelj, Igor Kononenko:  An Eficient Explanation of Individual Classifications using Game Theory, 
\emph{Journal of Machine Learning Research}, 11(1):1-18, 2010. 

Some references are available from \url{http://lkm.fri.uni-lj.si/rmarko/papers/}

}

\keyword{ package }
\keyword{ multivariate }
\keyword{ models }

\seealso{
\code{\link{explainVis}}
}
