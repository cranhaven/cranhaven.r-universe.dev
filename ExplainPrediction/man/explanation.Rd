\name{explanation}
\alias{explain}
\alias{prepareForExplanations}
\alias{explanationAverages}
\alias{explainVis}

\title{Explanation of predictions on instance and model level}
\description{
  Using general explanation methodology EXPLAIN or IME, the function \code{explainVis} explains
   predictions of given model and visualizes the  explanations.
  An explanation of a prediction is given for individual instances; aggregation of instance explanations 
  gives a model explanation. The details are given in the description and references.
  }
\usage{
explainVis(model, trainData, testData,  
   method=c("EXPLAIN", "IME"), classValue=1,
   fileType=c("none","pdf","eps","emf","jpg","png","bmp","tif","tiff"), 
   dirName=getwd(), fileName="explainVis", visLevel=c("both","model","instance"),
   explainType=c("WE","infGain","predDiff"), naMode=c("avg", "na"), 
   nLaplace=nrow(trainData), estimator=NULL,	
   pError=0.05, err=0.05, batchSize=40, maxIter=1000, 
   genType=c("rf", "rbf", "indAttr"), noAvgBins=20, 
   displayAttributes=NULL, modelVisCompact=FALSE, 
   displayThreshold=0.0, normalizeTo=0, 
   colors=c("navyblue", "darkred", "blue", "red", "lightblue", "orange"), 
   noDecimalsInValueName=2,
   modelTitle=ifelse(model$noClasses==0,"Explaining \%R\nmodel: \%M", 
      "Explaining \%R=\%V\\nmodel: \%M"), 
   modelSubtitle="Method: \%E, type: \%X", 
   instanceTitle=ifelse(model$noClasses==0, 
      "Explaining \%R\\ninstance: \%I, model: \%M",
      "Explaining \%R=\%V\\ninstance: \%I, model: \%M"), 
   instanceSubtitle=ifelse(model$noClasses==0, 
	  "Method: \%E\\nf(\%I)=\%P, true \%R=\%T",
      "Method: \%E, type: \%X\\nP(\%R=\%V)=\%P, true \%R=\%T"),
   recall=NULL) 		                                    
}
\arguments{
  \item{model}{ The model as returned by \code{\link{CoreModel}} function. }
  \item{trainData}{ Data frame with data, which is used to extract average explanations, discretization, 
        and other information needed for explanation of instances and model. Typically this is the data set 
        which was used to train the \code{model}.}
  \item{testData}{ Data frame with instances which will be explained. 
            The \code{testData} data frame shall contain the same columns as \code{trainData}, with possible exception
            of target variable, which can be omitted.}
  \item{method}{ The explanation method; two methods are available, EXPLAIN and IME. The EXPLAIN is much faster 
                and works for any number of attributes in the model,
          but cannot explain dependencies expressed disjunctively in the model (for details see references). 
         The IME can in principle explain any type of dependencies
          in the model. It uses sampling based method to avoid exhaustive search for dependencies and 
          works reasonably fast for up to a few dozen attributes in the model.}
    \item{classValue}{ For classification models this parameter determines for which class value the explanations will be generated.
                    The \code{classValue} can be given as a factor, character string or class index. 
                    By default the first class value is chosen.}
  \item{fileType}{The parameter determines the graphical format of the visualization file(s). 
           If \code{fileType="none"} (default) visualizations are generated in a
             graphical window. Other possible choices are \code{"pdf","eps","emf","jpg","png","bmp","tif"} and \code{"tiff"}. }         
 \item{dirName}{ A name of the folder where resulting visualization files will be saved if \code{fileType} other than \code{"none"} is chosen.}
 \item{fileName}{ A file name of the resulting visualization files, in case \code{fileType} other than \code{"none"} is chosen.}
  \item{visLevel}{ The level of explanations desired. If \code{visLevel="model"} the model explanation is 
           generated, meaning that instance explanations obtained on \code{trainData} are aggregated. 
           If \code{visLevel="instance"}  instance explanations are generated  for each row in testData. 
             The default value \code{visLevel="both"} generates both, the model explanation and explanations for the instances.}
   \item{explainType}{For method EXPLAIN this parameter determines how the prediction with knowledge about 
               given feature and prediction
               without knowledge of this feature are combined into the final explanation. 
               Values \code{"WE"}, \code{"infGain"}, and \code{"predDiff"} mean that the difference
               is interpreted as weight of evidence, information gain, or plain difference, respectively.
               For regression problem only the difference of predictions is available.}
 \item{naMode}{For method EXPLAIN this parameter determines how the impact of missing information about certain feature value is
              estimated. If \code{naMode="avg"}, the effect is estimated by the weighted average of predictions 
              over all possible feature's values.
               If \code{naMode="na"}, the effect is estimated by inserting NA value as feature value. 
               The \code{"na"} method is faster but we are 
               left to the mercy of adequate treatment of missing values in the function \code{\link{predict}} for a given model. }
   \item{nLaplace}{ For EXPLAIN method and classification problems the predicted probabilities are corrected with Laplace correction,
    pushing them away from 0 and 1 and towards uniform distribution. Larger values imply smaller effect. The default value is equal
    to the number of instances in \code{trainData}. The value 0 means that Laplace correction is not used and probabilities
    are estimated with relative frequency.}
    \item{estimator}{ The name of feature evaluation method used to greedily discretize attributes 
                  when averaging explanation over intervals.
                  The default value \code{NULL} means that \code{"ReliefFexpRank"} will be used in classification problems and 
                  \code{"RReliefFexpRank"} will be used in regression problems. See \code{\link{discretize}} for details.}
    \item{pError}{For method IME the estimated probability of an error in explanations. Together with
                  parameter \code{err} this determines the number of needed samples.}
    \item{err}{For method IME the parameter controls the size of tolerable error. 
               Together with parameter \code{pError} this determines the number of needed samples. 
               See the paper \emph{An Efficient Explanation of Individual Classifications using Game Theory} for details.}
    \item{batchSize}{For method IME the number of samples processed in batch mode for each explanation. Larger sizes cause
        less overhead in processing but may process more samples than required.}
     \item{maxIter}{The maximal number of iterations in IME method allowed for a single explanation.}
     \item{genType}{The type of data generator used to generate random part of instances in method IME. 
                  The generators from package \code{\link{semiArtificial-package}} are used: 
                  \code{"rf"} stands for random forest based generator, 
                  \code{"rbf"} invokes RBF network based generator, and
                  \code{"indAttr"} assumes independent attributes and generates values 
                  for each attribute independently.}
	 \item{noAvgBins}{For IME method the number of discretization bins used to present model level explanations
	  and average explanations.}
    \item{displayAttributes}{ The vector of attribute names which are visualized, subject to \code{displayThreshold}) and value \code{modelVisCompact}.
    The default value \code{displayThreshold=NULL} displays all attributes and their values.}
    \item{modelVisCompact}{ The logical value controlling if attribute values are displayed
     in model level visualization. The default value \code{modelVisCompact=FALSE} displays all values of
     attributes (subject to \code{displayThreshold}), and value \code{modelVisCompact=TRUE}
     displays only contributions on the level of attributes (without their values).} 
    \item{displayThreshold}{ The threshold value for absolute values of explanations 
     below which feature contributions are not displayed in instance and model explanation graphs. 
     The threshold applies after the values are normalized, see the explanation for parameter \code{normalizeTo}.
    The default value \code{displayThreshold=0} displays contributions of all attributes.}
    \item{normalizeTo}{ For instance level visualization the absolute values of feature contributions are 
    summed and normalized to the value of \code{normalizeTo}.
    In model level explanation the normalization depends  on parameter \code{modelVisCompact}. If its value is \code{TRUE},
    the absolute values of all feature explanations are summed up and normalized to \code{normalizeTo}, otherwise
    the absolute values of all feature values' contributions are summed up.
    The value of \code{normalizeTo} common in some areas ( e.g., in medicine) is 100. The default value 0 implies no normalization.
    The \code{displayThreshold} parameter refers to already normalized values.}
  \item{colors}{A vector with 6 colors names, giving 6 colors used in visualization (average positive impact of attribute, average negative impact of attribute, 
  positive instance explanation, negative instance explanation, average positive impact of attribute value, average negative impact of attribute value).
  If set to NULL sensible grayscale defaults are used i.e., (gray30,gray30,gray60,gray60,gray90,gray90). }
  \item{noDecimalsInValueName}{How many decimal places will numeric feature values use in visualizations.The default value is 2.}
  \item{modelTitle}{A character string for title template of model visualization. See the details. If \code{modelTitle=NULL} the title is not shown. }  
  \item{modelSubtitle}{A character string for subtitle template of model visualization. See the details. If \code{modelSubtitle=NULL} the subtitle is not shown. }     
  \item{instanceTitle}{A character string for title template of instance visualization. See the details. If \code{instanceTitle=NULL} the title is not shown. }  
  \item{instanceSubtitle}{A character string for subtitle template of instance visualization. See the details. If \code{instanceSubtitle=NULL} the subtitle is not shown. }       
   \item{recall}{If parameter is different from NULL, it shall contain the list invisibly returned by one of previous calls to function \code{explainVis}. In this case the function reuses already computed explanations,
  average explanations, discretization, etc.,  and only display data differently according to other supplied parameters. 
  In this case values of parameters \code{model}, \code{testData} and \code{classValue}should be identical to the original call.
  Values of parameters \code{trainData}, \code{method}, \code{naMode}, \code{explainType}, \code{nLaplace}, \code{estimator},
  \code{pError}, \code{err}, \code{batchSize}, \code{maxIter}, \code{genType}, and \code{noAvgBins}   are ignored.    
  The parameters that do matter in this case are the ones that affect the display of already precomputed
  results: \code{visLevel},\code{dirName}, \code{fileType}, 
  \code{displayAttributes}, \code{modelVisCompact}, \code{displayThreshold}, \code{normalizeTo},
  \code{colors}, \code{noDecimalsInValueName}, \code{modelTitle}, \code{modelSubtitle}, \code{instanceTitle}, and \code{instanceSubtitle}.
  % The same information for each call is also stored in the variable .lastExplainVis, which is put into the global environment. 
  }                                    
}
\details{
The function \code{explainVis} generates explanations and their visualizations given the trained model, 
its training data, and data for which we want explanations. This is the frontend explanation function which takes
care of everything, internally calling other functions.
The produced visualizations are output to a graphical device or saved to a file. 
If one requires internal information about the explanations, they are returned invisibly. % and stored to variable .lastExplainVis. 
Separate calls to internal functions (\code{explain}, \code{ime}, 
\code{prepareForExplanations}, and \code{explanationAverages}) are also possible.

In the model explanation all feature values  of nominal attributes and intervals of numeric attributes are visualized, as
well as weighted summary over all these values. 
In the instance level visualizations the contributions of each feature are presented (thick bars) as well as average contributions of these
feature values in the \code{trainData} (thin bars above them). For details see the references below.

The titles and subtitles of model and instance explanation graphs use templates which allows insertion of the following values:
\itemize{
 \item 	Response variable: \%R
\item Selected class value for explanation (in case of classification): \%V 
\item Type of model: \%M
\item Explanation method (see parameter \code{method}):: \%E
\item Explanation type (only for method EXPLAIN): \%X
}
Title and subtitle of instance explanation graphs can additionally use the following information:
\itemize{
\item Instance name (extracted from \code{\link{row.names}} in \code{testData}): \%I
\item Predicted value/probability of the response: \%P 
\item True (class) value of the response: \%T
}
Default templates for regression and classification models are provided. For example, the default template for title of model explanation is
"Explaining \%R=\%V\\nmodel: \%M", meaning that information about response variable, selected class value, and model are displayed in the title. 
 }

\value{
 The function \code{explainVis} generates explanations and saves their visualizations to a file or 
 outputs them to graphical device,  based on the value of \code{fileType}. It invisibly returns a list with three components containing
 explanations, average explanations, and additional data like discretization used and data generator.
 The main ingredients of these three components are:
 \itemize{
 \item \code{expl}, a matrix of generated explanations  (of size \code{dim(testData)}), 
 \item \code{pCXA}, a vector of predictions,
 \item \code{stddev}, (for method IME only) a matrix with standard deviations of explanations,
 \item \code{noIter}, (for method IME only) a matrix with number of iterations executed for each explanation,
 \item  \code{discPoints}, (for method EXPLAIN only) a list containing values of discrete features 
        or centers of discretization intervals for numeric features,
 \item \code{pAV}, (for method EXPLAIN only) a list with probabilities for discrete values or 
       discretization intervals in case of numeric features,
 \item \code{discretization}, a list with discretization intervals output by \code{\link{discretize}} function,
       used in estimating averages and model based explanations,
\item \code{avNames}, a list containing the names of discrete values/intervals,
\item \code{generator}, (for IME method only) a generator used to generate random part of instances in IME method,
\item \code{explAvg}, a list with several components giving average explanations on the \code{trainingData}.
Averages are given for 
attributes, their values (for discrete attributes) and discretization intervals (for numeric features). 
These average explanations are used in visualization to give impression how the model works on average. This can be contrasted 
with explanation for the specific instance.
}
 }
\references{
Marko Robnik-Sikonja, Igor Kononenko: Explaining Classifications For Individual Instances.
\emph{IEEE Transactions on Knowledge and Data Engineering}, 20:589-600, 2008

Erik Strumbelj, Igor Kononenko, Igor, Marko Robnik-Sikonja: Explaining Instance Classifications with Interactions of 
Subsets of Feature Values. \emph{Data and Knowledge Engineering}, 68(10):886-904, Oct. 2009

Erik Strumbelj, Igor Kononenko:  An Efficient Explanation of Individual Classifications using Game Theory, 
\emph{Journal of Machine Learning Research}, 11(1):1-18, 2010. 

Marko Robnik-Sikonja, Igor Kononenko: Discretization of continuous attributes using ReliefF.
 \emph{Proceedings of ERK'95}, B149-152, Ljubljana, 1995

Some references are available from \url{http://lkm.fri.uni-lj.si/rmarko/papers/}
}

\author{ Marko Robnik-Sikonja }

\seealso{
\code{\link{CORElearn}},
\code{\link{predict.CoreModel}},
\code{\link{attrEval}},
\code{\link{discretize}},
\code{\link{semiArtificial-package}}
}

\examples{
require(CORElearn)
# use iris data set, split it randomly into a training and testing set
trainIdxs <- sample(x=nrow(iris), size=0.7*nrow(iris), replace=FALSE)
testIdxs <- c(1:nrow(iris))[-trainIdxs]
# build random forests model with certain parameters
modelRF <- CoreModel(Species ~ ., iris[trainIdxs,], model="rf",
              selectionEstimator="MDL",minNodeWeightRF=5,
              rfNoTrees=100, maxThreads=1)

# generate model explanation and visualization
# turn on history in the visualization window to see all graphs
explainVis(modelRF, iris[trainIdxs,], iris[testIdxs,], method="EXPLAIN",visLevel="both",
           fileType="none", naMode="avg", explainType="WE", classValue=1) 
\dontrun{
#store instance explanations in grayscale to file in PDF format
explainVis(modelRF, iris[trainIdxs,], iris[testIdxs,], method="EXPLAIN", visLevel="instance",
           fileType="pdf", naMode="avg", explainType="WE", classValue=1, colors=NULL) 
destroyModels(modelRF) # clean up

# build a regression tree 
trainReg <- regDataGen(100)
testReg <- regDataGen(20)
modelRT <- CoreModel(response~., trainReg, model="regTree", modelTypeReg=1)
# generate both model and instance level explanation using defaults
explainVis(modelRT, trainReg, testReg) 
destroyModels(modelRT) #clean up
}
}
\keyword{ models }
\keyword{ regression }
\keyword{ classif }
