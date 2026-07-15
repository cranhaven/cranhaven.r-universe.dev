\name{treeEnsemble}
\alias{treeEnsemble}
\alias{indAttrGen}
\title{A data generator based on forest}
\description{
  Using given \code{formula} and \code{data} the method \code{treeEnsemble} builds a tree ensemble and turns it into a data generator, which can be used with \code{\link{newdata}} method to generate 
  semi-artificial data. The methods supports classification, regression, and unsupervised data, depending on the input and parameters.
  The method \code{indAttrGen} generates data from the same distribution as the input data, but 
  assuming conditionally independent attributes. 
}
\usage{
 treeEnsemble(formula, dataset, noTrees = 100, minNodeWeight=2, noSelectedAttr=0, 
  	problemType=c("byResponse","classification", "regression","density"),
    densityData=c("leaf", "topDown", "bottomUp","no"),
    cdfEstimation = c("ecdf","logspline","kde"), 
	densitySplitMethod=c("balancedSplit","randomSplit","maxVariance"),
    estimator=NULL,...) 

indAttrGen(formula, dataset, cdfEstimation = c("ecdf","logspline","kde"), 
           problemType="byResponse") 
}
\arguments{
  \item{formula}{ A formula specifying the response and variables to be modeled. }
  \item{dataset}{ A data frame with training data. }
  \item{noTrees}{ The number of trees in the ensemble.}
  \item{minNodeWeight}{The minimal number of instances in a tree leaf.}
  \item{noSelectedAttr}{Number of randomly selected attributes in each node which are considered as possible splits. In general this should be a positive integer, 
                    but values 0, -1, and -2 are also possible. 
                     The default value is \code{noSelectedAttr=0}, which causes random selection of integer rounded \eqn{\sqrt{a}}{sqrt(a)} attributes, where $a$ is the number of all attributes. 
                     Value -1 means that \eqn{1+\log_2{a}}{1+log2(a)} attributes are selected and value -2 means that all attributes are selected.}   
  \item{problemType}{The type of the problem modeled: classification, regression, or unsupervised (density estimation). The default value  \code{"byResponse"} indicates that 
                     the problem type is deducted based on \code{formula} and \code{data}. }
  \item{densityData}{The type of generator data and place where new instances are generated: in the leafs, top down from the root of the tree to the leaves, bottom up from
                     the leaves to root. In case of value \code{"no"} the ensemble contains no generator data and can be used as an ordinary ensemble predictor (although 
                     probably slow, as it is written entirely in R).}
  \item{cdfEstimation}{The manner values are generated and the type of data stored in the generator: \code{"ecdf"} indicates values are generated from empirical cumulative 
                       distributions stored for each variable separately; \code{"logspline"} means that value distribution is modeled with logsplines, and \code{"kde"} 
                       indicates that Gaussian kernel density estimation is used.}
  \item{densitySplitMethod}{In case \code{problemType="density"} the parameters determines the criteria for selection of split in the density tree. Possible choices are 
                       balanced (a split value is chosen in such a way that the split is balanced), random (split value is chosen randomly) and maxVariance (split with 
                       maximal variance is chosen).}
  \item{estimator}{The attribute estimator used to select the node split in classification and regression trees. Function \code{attrEval} from \code{CORElearn} package is 
                   used, so the values have to be compatible with that function. The default value \code{NULL} chooses Gini index in case of classification problems and MSE 
                   (mean squared error in resulting splits) in case of regression.}
  \item{...}{Further parameters to be passed onto probability density estimators.}
}
\details{
  Parameter \code{formula} is used as a mechanism to select features (attributes)
  and the prediction variable (response) from the data. Only simple terms can be used and
  interaction terms are not supported. The simplest way is
  to specify just the response variable using e.g. \code{class ~ .}. For unsupervised problems all variables can be selected using formula \code{ ~ .}.
  See examples below.

  A forest of trees is build using R code. The base models of the ensemble are classification, regression or density trees with additional information stored at the 
  appropriate nodes. New data can be generated using \code{\link{newdata}} method.
  
  The method \code{indAttrGen} generates data from the same distribution as the input data (provided in 
  \code{dataset}), but assumes conditional independence of attributes. This assumption makes the generated data
  a simple baseline generator. Internally, the method calls \code{treeEnsemble} with parameters 
  \code{noTrees=1}, \code{minNodeWeight=nrow(dataset)}, \code{densityData="leaf"}.  
 }
 
\value{
  The created model is returned with additional data stored as a list and also in the trees. The model can be used with function \code{\link{newdata}} to generate new values.
  }
\references{
Marko Robnik-Sikonja: Not enough data? Generate it!. \emph{Technical Report, University of Ljubljana, Faculty of Computer and Information Science}, 2014

Other references are available from \url{http://lkm.fri.uni-lj.si/rmarko/papers/}
}

\author{ Marko Robnik-Sikonja}

\seealso{
\code{\link{newdata}}.
}

\examples{
# use iris data set, split into training and testing, inspect the data
set.seed(12345)
train <- sample(1:nrow(iris),size=nrow(iris)*0.5)
irisTrain <- iris[train,]
irisTest <- iris[-train,]

# inspect properties of the original data
plot(iris[,-5], col=iris$Species)
summary(iris)

# create tree ensemble generator for classification problem
irisGenerator<- treeEnsemble(Species~., irisTrain, noTrees=10)

# use the generator to create new data
irisNew <- newdata(irisGenerator, size=200)

#inspect properties of the new data
plot(irisNew[,-5], col = irisNew$Species) # plot generated data
summary(irisNew)

\dontrun{
# create tree ensemble generator for unsupervised problem
irisUnsupervised<- treeEnsemble(~.,irisTrain[,-5], noTrees=10)
irisNewUn <- newdata(irisUnsupervised, size=200)
plot(irisNewUn) # plot generated data
summary(irisNewUn)

# create tree ensemble generator for regression problem
CO2gen<- treeEnsemble(uptake~.,CO2, noTrees=10)
CO2New <- newdata(CO2gen, size=200)
plot(CO2) # plot original data
plot(CO2New) # plot generated data
summary(CO2)
summary(CO2New)
}

}




\keyword{ datagen }
\keyword{ multivariate }
\keyword{ classif }
