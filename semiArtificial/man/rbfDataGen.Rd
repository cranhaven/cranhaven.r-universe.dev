\name{rbfDataGen}
\alias{rbfDataGen}
\title{A data generator based on RBF network}
\description{
  Using given \code{formula} and \code{data} the method builds a RBF network and extracts its properties thereby preparing a data generator which can be used 
  with \code{\link{newdata.RBFgenerator}} method to generate semi-artificial data.
}
\usage{
 rbfDataGen(formula, data, eps=1e-4, minSupport=1, 
            nominal=c("encodeBinary","asInteger")) 
}
\arguments{
  \item{formula}{ A formula specifying the response and variables to be modeled. }
  \item{data}{ A data frame with training data. }
  \item{eps}{ The minimal probability considered in data generator to be larger than 0.}
  \item{minSupport}{The minimal number of instances defining a Gaussian kernel to copy the kernel to the data generator.}
  \item{nominal}{The way how to treat nominal features. The option \code{"asInteger"} converts factors 
  into integers and treats them as numeric features. The option \code{"encodeBinary"} converts each nominal 
  attribute into a set of binary features, which encode the nominal value, e.g., for three valued attribute 
  three binary attributes are constructed, each encoding a presence of one nominal value with 0 or 1.}
}
\details{
  Parameter \code{formula} is used as a mechanism to select features (attributes)
  and the prediction variable (response, class). Only simple terms can be used and
  interaction terms are not supported. The simplest way is
  to specify just the response variable using e.g. \code{class ~ .}. See examples below.

  A RBF network is build using \code{rbfDDA} from \href{https://CRAN.R-project.org/package=RSNNS}{RSNNS} package. 
  The learned Gaussian kernels are extracted and used in data generation with 
  \code{\link{newdata.RBFgenerator}} method.
 
 }
 
\value{
  The created model is returned as a structure of class \code{RBFgenerator}, containing the following items:
\item{noGaussians}{The number of extracted Gaussian kernels.}
\item{centers}{A matrix of Gaussian kernels' centers, with one row for each Gaussian kernel.}
\item{probs}{A vector of kernel probabilities. Probabilities are defined as relative frequencies of training set instances with  maximal activation in the given kernel.}
\item{unitClass}{A vector of class values, one for each kernel.}
\item{bias}{A vector of kernels' biases, one for each kernel. The bias is multiplied by the kernel activation to produce output value of given RBF network unit.}
\item{spread}{A matrix of estimated variances for the kernels, one row for each kernel. 
The j-th value in i-th row represents the variance of training instances for j-th attribute with maximal activation 
in i-th Gaussian.}
\item{gNoActivated}{A vector containing numbers of training instances with maximal activation in each kernel.}
\item{noAttr}{The number of attributes in training data.}
\item{datNames}{A vector of attributes' names.}
\item{originalNames}{A vector of original attribute names.}
\item{attrClasses}{A vector of attributes' classes (i.e., data types like \code{numeric} or \code{factor}).}
\item{attrLevels}{A list of levels for discrete attributes (with class \code{factor}).}
\item{attrOrdered}{A vector of type logical indicating whether the attribute is \code{ordered} (only possible for attributes of type \code{factor}.}
\item{normParameters}{A list of parameters for normalization of attributes to [0,1].}
\item{noCol}{The number of columns in the internally generated data set.}
\item{isDiscrete}{A vector of type logical, each value indicating whether a respective attribute is discrete.}
\item{noAttrGen}{The number of attributes to generate.}
\item{nominal}{The value of parameter \code{nominal}.}
}
\references{
Marko Robnik-Sikonja: Not enough data? Generate it!. \emph{Technical Report, University of Ljubljana, Faculty of Computer and Information Science}, 2014

Other references are available from \url{http://lkm.fri.uni-lj.si/rmarko/papers/}
}

\author{ Marko Robnik-Sikonja}

\seealso{
\code{\link{newdata.RBFgenerator}}.
}

\examples{
# use iris data set, split into training and testing, inspect the data
set.seed(12345)
train <- sample(1:nrow(iris),size=nrow(iris)*0.5)
irisTrain <- iris[train,]
irisTest <- iris[-train,]

# inspect properties of the original data
plot(irisTrain, col=irisTrain$Species)
summary(irisTrain)

# create rbf generator
irisGenerator<- rbfDataGen(Species~.,irisTrain)

# use the generator to create new data
irisNew <- newdata(irisGenerator, size=200)

#inspect properties of the new data
plot(irisNew, col = irisNew$Species) #plot generated data
summary(irisNew)
}

\keyword{ datagen }
\keyword{ multivariate }
\keyword{ classif }
