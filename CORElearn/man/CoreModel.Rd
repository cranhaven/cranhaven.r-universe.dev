\name{CoreModel}
\alias{CoreModel}
\alias{cvCoreModel}
\title{Build a classification or regression model }
\description{
  Builds a classification or regression model from the \code{data} and \code{formula} with given parameters.
  Classification models available are
\itemize{
\item random forests, possibly with local weighing of basic models (parallel execution on several cores),
\item decision tree with constructive induction in the inner nodes and/or models in the leaves,
\item kNN and weighted kNN with Gaussian kernel,
\item naive Bayesian classifier.
}
Regression models:
\itemize{
\item regression trees with constructive induction in the inner nodes and/or models in the leaves,
\item linear models with pruning techniques,
\item locally weighted regression,
\item kNN and weighted kNN with Gaussian kernel.
}
Function \code{cvCoreModel} applies cross-validation to estimate predictive performance of the model.
}
\usage{
  CoreModel(formula, data,
       model=c("rf","rfNear","tree","knn","knnKernel","bayes","regTree"),
       costMatrix=NULL,...)
  cvCoreModel(formula, data,
       model=c("rf","rfNear","tree","knn","knnKernel","bayes","regTree"),
       costMatrix=NULL, folds=10, stratified=TRUE, returnModel=TRUE, ...)

}
\arguments{
  \item{formula}{ Either a formula specifying the attributes to be evaluated and the target variable, or a name of target variable, or an index of target variable. }
  \item{data}{ Data frame with training data. }
  \item{model}{ The type of model to be learned. }
  \item{costMatrix}{ Optional misclassification cost matrix used with certain models. }
  \item{folds}{ An integer, specifying the number of folds to use in cross-validation of model. }
  \item{stratified}{ A boolean specifying if cross-valiadation is to be stratified fpr classification problems, i.e. shall all folds have the same distribution of class values. }
  \item{returnModel}{ If \code{TRUE} the function \code{cvCoreModel} estimates predictive performance using cross-validation and returns the model build on the whole data set. 
        If \code{returnModel=FALSE} the function only evaluates the model using cross-validation.  }
  \item{... }{ Options for building the model. See \code{\link{helpCore}}. }
  
}
\details{
   The parameter \code{formula} can be interpreted in three ways, where the formula interface is the most elegant one, 
  but inefficient and inappropriate for large data sets. See also examples below. As \code{formula} one can specify:
\describe{ 
  \item{an object of class \code{formula}}{used as a mechanism to select features (attributes) 
  and prediction variable (class). Only simple terms can be used and 
  interaction expressed in formula syntax are not supported. The simplest way is
  to specify just response variable: \code{class ~ .}.
  In this case all other attributes in the data set are evaluated. Note that formula interface is not appropriate for data sets with
  large number of variables.}
  \item{a character vector}{specifying the name of target variable, all the other columns in data frame \code{data} are used as predictors.}
  \item{an integer}{specifying the index of of target variable in data frame \code{data}, all the other columns are used as predictors.}
}

  Parameter \bold{model} controls the type of the constructed model. There are several possibilities:
  \describe{
     \item{\code{"rf"}}{ random forests classifier as defined by (Breiman, 2001) with some extensions, }
     \item{\code{"rfNear"}}{ random forests classifier with basic models weighted locally (Robnik-Sikonja, 2005), }
     \item{\code{"tree"}}{ decision tree with constructive induction in the inner nodes and/or models in the leaves, }
     \item{\code{"knn"}}{ k nearest neighbors classifier, }
     \item{\code{"knnKernel"}}{ weighted k nearest neighbors classifier with distance taken into account through Gaussian kernel, }
     \item{\code{"bayes"}}{ naive Bayesian classifier, }
     \item{\code{"regTree"}}{ regression trees with constructive induction in inner nodes and/or models in leaves
     controlled by modelTypeReg parameter.
     Models used in leaves of the regression tree can also be used as stand-alone regression models 
     using option minNodeWeightTree=Inf (see examples below):
     \itemize{
        \item linear models with pruning techniques
        \item locally weighted regression
        \item kNN and kNN with Gaussian kernel.
      }
      }
   }

  There are many additional parameters \bold{\dots } available which are used by different models.
  Their list and description is available by calling \code{\link{helpCore}}. Evaluation of attributes is covered
  in function \code{\link{attrEval}}.

  The optional parameter \bold{ costMatrix } can provide nonuniform cost matrix for classification problems. For regression
  problem this parameter is ignored. The format of the matrix is costMatrix(true class, predicted class).
  By default uniform costs are assumed, i.e.,  costMatrix(i, i) = 0, and costMatrix(i, j) = 1, for i not equal to j.
}
\value{
  The created model is not returned as a R structure. It is stored internally
  in the package memory space and only its pointer (index) is returned.
  The maximum  number of models that can be stored simultaneously
  is a parameter of the initialization function \code{initCore} and
  defaults to 16384. Models, which are not needed, may be deleted in order
  to free the memory using function \code{destroyModels}.
  By referencing the returned model, any of the stored models may be
  used for prediction with \code{\link{predict.CoreModel}}.
  What the function actually returns is a list with components:
    \item{modelID}{ index of internally stored model, }
    \item{terms}{ description of prediction variables and response, }
    \item{class.lev}{ class values for classification problem, null for regression problem, }
    \item{model}{ the type of model used, see parameter \code{model}, }
    \item{formula}{ the \code{formula} parameter passed. }
  The function \code{cvCoreModel} evaluates the model using cross-validation and function \code{\link{modelEval}} to return
   these additional components:
  \item{avgs}{A vector with average values of each evaluation metric obtained from \code{\link{modelEval}}. }
  \item{stds}{A vector with standard deviations of each evaluation metric from \code{\link{modelEval}}. }
  \item{evalList}{A list, where each component is an evaluation metric from \code{\link{modelEval}}. Each component contains results of cross-validated runs.}
   In case \code{returnModel=FALSE} the function only returns the above three components are keeps no model.
 }
\references{
Marko Robnik-Sikonja, Igor Kononenko: Theoretical and Empirical Analysis of ReliefF and RReliefF.
    \emph{Machine Learning Journal}, 53:23-69, 2003

Leo Breiman: Random Forests. \emph{Machine Learning Journal}, 45:5-32, 2001

Marko Robnik-Sikonja: Improving Random Forests.
      In J.-F. Boulicaut et al.(Eds): \emph{ECML 2004, LNAI 3210}, Springer, Berlin, 2004, pp. 359-370

Marko Robnik-Sikonja: CORE - a system that predicts continuous variables.
    \emph{Proceedings of ERK'97} , Portoroz, Slovenia, 1997

Marko Robnik-Sikonja, Igor Kononenko: Discretization of continuous attributes using ReliefF.
 \emph{Proceedings of ERK'95}, B149-152, Ljubljana, 1995

Majority of these references are available from \url{http://lkm.fri.uni-lj.si/rmarko/papers/}
}

\author{ Marko Robnik-Sikonja, Petr Savicky }

\seealso{
\code{\link{CORElearn}},
\code{\link{predict.CoreModel}},
\code{\link{modelEval}},
\code{\link{attrEval}},
\code{\link{helpCore}},
\code{\link{paramCoreIO}}.
}

\examples{
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
mEval <- modelEval(modelRF, iris[["Species"]][testIdxs], pred$class, pred$prob)
print(mEval) # evaluation of the model
# visualization of individual predictions and the model
\dontrun{
require(ExplainPrediction)
explainVis(modelRF, iris[trainIdxs,], iris[testIdxs,], method="EXPLAIN",
           visLevel="model", problemName="iris", fileType="none", 
           classValue=1, displayColor="color") 
# turn on the history in visualization window to see all instances
explainVis(modelRF, iris[trainIdxs,], iris[testIdxs,], method="EXPLAIN",
           visLevel="instance", problemName="iris", fileType="none", 
           classValue=1, displayColor="color") 
}
destroyModels(modelRF) # clean up


# build decision tree with naive Bayes in the leaves
# more appropriate for large data sets one can specify just the target variable
modelDT <- CoreModel("Species", iris, model="tree", modelType=4)
print(modelDT)
destroyModels(modelDT) # clean up


# build regression tree similar to CART
instReg <- regDataGen(200)
modelRT <- CoreModel(response~., instReg, model="regTree", modelTypeReg=1)
print(modelRT)
destroyModels(modelRT) # clean up

# build kNN kernel regressor by preventing tree splitting
modelKernel <- CoreModel(response~., instReg, model="regTree",
                    modelTypeReg=7, minNodeWeightTree=Inf)
print(modelKernel)
destroyModels(modelKernel) # clean up

\dontrun{
# A more complex example 
# Test accuracy of random forest predictor with 20 trees on iris data
# using 10-fold cross-validation.
ncases <- nrow(iris)
ind <- ceiling(10*(1:ncases)/ncases)
ind <- sample(ind,length(ind))
pred <- rep(NA,ncases)
fit <- NULL
for (i in unique(ind)) {
    # Delete the previous model, if there is one.
    fit <- CoreModel(Species ~ ., iris[ind!=i,], model="rf", 
                     rfNoTrees=20, maxThreads=1)
    pred[ind==i] <- predict(fit, iris[ind==i,], type="class")
    if (!is.null(fit)) destroyModels(fit) # dispose model no longer needed
 
}
table(pred,iris$Species)
}
# a simpler way to estimate performance using cross-validation
model <- cvCoreModel(Species ~ ., iris, model="rf", rfNoTrees=20, 
                    folds=10, stratified=TRUE, returnModel=TRUE,
                    maxThreads=1)    
model$avgs           

}

\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ tree }
\keyword{ multivariate }
\keyword{ loess }
\keyword{ classif }
