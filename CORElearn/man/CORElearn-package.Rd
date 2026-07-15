\name{CORElearn-package}
\alias{CORElearn-package}
\alias{CORElearn}
\docType{package}
\title{R port of CORElearn }
\description{
The package CORElearn is an R port of CORElearn data mining system.
It provides various classification and regression models
as well as algorithms for feature selection and evaluation.
Several algorithms support parallel multithreaded execution via OpenMP (see details in function descriptions)., 
It is possible to run many functions outside the R environment.
The description and source code is available on the package web site
\url{http://lkm.fri.uni-lj.si/rmarko/software/}.
}
\details{
The main functions are 
\itemize{
\item \code{\link{CoreModel}} which constructs classification  or regression model.
\itemize{
\item Classification models available:
\itemize{
\item random forests with optional local weighing of basic models
\item decision tree with optional constructive induction in the inner nodes and/or models in the leaves
\item kNN and kNN with Gaussian kernel, 
\item naive Bayes.
}
\item Regression models:
\itemize{
\item regression trees with optional constructive induction in the inner nodes and/or models in the leaves, 
\item linear models with pruning techniques
\item locally weighted regression
\item kNN and kNN with Gaussian kernel.
}
}
\item \code{\link{predict.CoreModel}}  predicts with classification model labels and probabilities of new instances.
For regression models it returns the predicted function value.
\item \code{\link{plot.CoreModel}} graphically visualizes trees and random forest models
\item \code{\link{modelEval}} computes some statistics from predictions  
\item \code{\link{attrEval}}  evaluates the quality of the attributes (dependent variables)
with the selected heuristic method.  
Feature evaluation algorithms are various variants of Relief algorithms (ReliefF, RReliefF,
cost-sensitive ReliefF, etc), gain ratio, gini-index, MDL, DKM, information gain, MSE, MAE, etc.
\item \code{\link{ordEval}} evaluates ordinal attributes with ordEval algorithm and visualizes them with \code{\link{plot.ordEval}}, 
\item \code{\link{infoCore}}  outputs certain information about CORElearn methods,
\item \code{\link{helpCore}} prints short description of a given parameter,
\item \code{\link{paramCoreIO}} reads/writes parameters for given model from/to file, 
\item \code{\link{versionCore}} outputs version of the package from underlying C++ library.
}
Some of the internal structures of the C++ part are described in \code{\link{CORElearn-internal}}.

For an automatically generated list of functions use \code{help(package=CORElearn)} or 
\code{library(help=CORElearn)}.

ut this feature is currently not supported on all platforms and may interfere with other means of parallelization used in R, like package paralell.
It is tested to works on Windows, Linux, and Mac.

For certain platforms multithreaded execution is not supported, since current set of compilers 
at CRAN do not fully support OpenMP. Also note that OpenMP execution may interfere with other means of parallelization on certain platforms. E.g.,
interference with package parallel is reported on Windows which can be prevented by setting parameter maxThreads=1.
For platforms other than Linux, Windows, and OsX to support multithreading it is possible to recompile the package with appropriate tools and compilers
(modify Makefile or Makefile.win in src folder, or consult authors).   
}
\author{
Marko Robnik-Sikonja, Petr Savicky
}
\references{
Marko Robnik-Sikonja, Igor Kononenko: Theoretical and Empirical Analysis of ReliefF and RReliefF.
    \emph{Machine Learning Journal}, 53:23-69, 2003
    
Marko Robnik-Sikonja: Improving Random Forests. 
      In J.-F. Boulicaut et al.(Eds): \emph{ECML 2004, LNAI 3210}, Springer, Berlin, 2004, pp. 359-370

Marko Robnik-Sikonja, Koen Vanhoof: Evaluation of ordinal attributes at value level. 
     \emph{Knowledge Discovery and Data Mining}, 14:225-243, 2007    

Marko Robnik-Sikonja: Experiments with Cost-sensitive Feature Evaluation. 
   In Lavrac et al.(eds): \emph{Machine Learning, Proceedings of ECML 2003}, Springer, Berlin, 2003, pp. 325-336
   
Majority of these references are available also from \url{http://lkm.fri.uni-lj.si/rmarko/papers/}

}

\keyword{ package }
\keyword{ datasets }
\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ tree }
\keyword{ multivariate }
\keyword{ loess }
\keyword{ classif }

\seealso{
\code{\link{CoreModel}},
\code{\link{predict.CoreModel}},
\code{\link{plot.CoreModel}},
\code{\link{modelEval}},
\code{\link{attrEval}},
\code{\link{ordEval}},
\code{\link{plot.ordEval}},
\code{\link{helpCore}}, 
\code{\link{paramCoreIO}},
\code{\link{infoCore}},
\code{\link{versionCore}},
\code{\link{CORElearn-internal}},
\code{\link{classDataGen}},
\code{\link{regDataGen}},
\code{\link{ordDataGen}}.
}
\examples{
# load the package
library(CORElearn) 
cat(versionCore(),"\n")

# use iris data set
trainIdxs <- sample(x=nrow(iris), size=0.7*nrow(iris), replace=FALSE)
testIdxs <- c(1:nrow(iris))[-trainIdxs]

# build random forests model with certain parameters
# setting maxThreads to 0 or more than 1 forces 
# utilization of several processor cores 
modelRF <- CoreModel(Species ~ ., iris[trainIdxs,], model="rf",
              selectionEstimator="MDL",minNodeWeightRF=5,
              rfNoTrees=100, maxThreads=1)

print(modelRF) # simple visualization, test also others with function plot

# prediction on testing set
pred <- predict(modelRF, iris[testIdxs,], type="both") 

# compute statistics
mEval <- modelEval(modelRF, iris[["Species"]][testIdxs], pred$class, pred$prob)
print(mEval) 

\dontrun{
# explain predictions on the level of model and individual instances
require(ExplainPrediction)
explainVis(modelRF, iris[trainIdxs,], iris[testIdxs,], method="EXPLAIN",
           visLevel="model", problemName="iris", fileType="none", 
           classValue=1, displayColor="color") 
# turn on the history in visualization window to see all instances
explainVis(modelRF, iris[trainIdxs,], iris[testIdxs,], method="EXPLAIN",
           visLevel="instance", problemName="iris", fileType="none", 
           classValue=1, displayColor="color") 
}
# Clean up, otherwise the memory is still taken
destroyModels(modelRF) # clean up

 
# evaluate features in given data set with selected method
# instead of formula interface one can provide just 
# the name or index of target variable
estReliefF <- attrEval("Species", iris, 
                       estimator="ReliefFexpRank", ReliefIterations=30)
print(estReliefF)
    
# evaluate ordered features with ordEval
profiles <- ordDataGen(200)
est <- ordEval(class ~ ., profiles, ordEvalNoRandomNormalizers=100)
# print(est)  

}
