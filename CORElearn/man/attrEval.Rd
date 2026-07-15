\name{attrEval}
\alias{attrEval}
\title{ Attribute evaluation }
\description{
  The method evaluates the quality of the features/attributes/dependent variables
   specified by the formula with the selected heuristic method.  
   Feature evaluation algorithms available for classification problems 
   are various variants of Relief and ReliefF algorithms (ReliefF,
   cost-sensitive ReliefF, \dots), and impurity-based algorithms (information gain, gain ratio, gini-index, MDL, DKM, etc).
   For regression problems there are RREliefF, MSEofMean, MSEofModel, MAEofModel, ...
   Parallel execution on several cores is supported for speedup.
}
\usage{
  attrEval(formula, data, estimator, costMatrix = NULL, 
           outputNumericSplits=FALSE, ...)
}
\arguments{
  \item{formula}{ Either a formula specifying the attributes to be evaluated and the target variable, 
                  or a name of target variable, or an index of target variable. }
  \item{data}{ Data frame with evaluation data. }
  \item{estimator}{ The name of the evaluation method.}
  \item{costMatrix}{ Optional cost matrix used with certain estimators. }
  \item{outputNumericSplits}{ Controls of the output contain the best split point for numeric attributes. 
                 This is only sensible for impurity based estimators (like information gain, gini, MDL, gain ratio, etc. in classification, 
                 and MSEofMean in regression). The default value of parameter 
                 \code{binaryEvaluateNumericAttributes = TRUE} shall not be modified in this case. If the value of 
                 \code{outputNumericSplits = TRUE}, the output is a list with attribute evaluations and numeric attributes' splits 
                 (instead of a single vector with evaaluations). See the returned value description.}
  \item{\dots }{ Additional options used by specific evaluation methods as described in \code{\link{helpCore}}.}
}
\details{
  The parameter \code{formula} can be interpreted in three ways, where the formula interface is the most elegant one, 
  but inefficient and inappropriate for large data sets.   See also examples below. As \code{formula} one can specify:
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
  The optional parameter \bold{ costMatrix } can provide nonuniform cost matrix to certain
  cost-sensitive measures (ReliefFexpC, ReliefFavgC, ReliefFpe, ReliefFpa, ReliefFsmp,GainRatioCost,     
  DKMcost, ReliefKukar, and MDLsmp). For other measures this parameter is ignored.    
  The format of the matrix is costMatrix(true class, predicted class). 
  By default a uniform costs are assumed, i.e.,  costMatrix(i, i) = 0, and costMatrix(i, j) = 1, for i not equal to j.
  
  The \bold{estimator} parameter selects the evaluation heuristics.  For classification problem it 
  must be one of the names returned by \code{infoCore(what="attrEval")} and for 
  regression problem it must be one of the names returned by \code{infoCore(what="attrEvalReg")}
  Majority of these feature evaluation measures are described in the references given below,
  here only a short description is given. For classification problem they are
  \describe{
    \item{"ReliefFequalK"}{ReliefF algorithm where k nearest instances have equal weight. }
    \item{"ReliefFexpRank"}{ReliefF algorithm where k nearest instances have weight exponentially decreasing with 
           increasing rank. Rank of nearest instance is determined by the increasing (Manhattan) distance from the selected instance.
           This is a default choice for methods taking conditional dependencies among the attributes into account. }
    \item{"ReliefFbestK"}{ReliefF algorithm where all possible k (representing k nearest instances)
     are tested and for each feature the highest score is returned. Nearest instances have equal weights. }
    \item{"Relief"}{Original algorithm of Kira and Rendel (1991) working on two class problems. }
    \item{"InfGain"}{Information gain. }
    \item{"GainRatio"}{Gain ratio, which is normalized information gain to prevent bias to multi-valued attributes.}
    \item{"MDL"}{Acronym for Minimum Description Length, presents method introduced in (Kononenko, 1995) 
          with favorable bias for multi-valued and multi-class problems. Might be the best method
          among those not taking conditional dependencies into account. }
    \item{"Gini"}{Gini-index. }
    \item{"MyopicReliefF"}{Myopic version of ReliefF resulting from assumption of no local dependencies and 
           attribute dependencies upon class. }
    \item{"Accuracy"}{Accuracy of resulting split. }
    \item{"ReliefFmerit" }{ReliefF algorithm where for each random instance the merit of each attribute is 
          normalized by the sum of differences in all attributes.}
    \item{"ReliefFdistance"}{ReliefF algorithm where k nearest instances are weighed  directly with its 
             inverse distance from the selected instance. Usually using ranks instead of distance
             as in \code{ReliefFexpRank} is more effective. }
    \item{"ReliefFsqrDistance"}{ReliefF algorithm where k nearest instances are weighed  with its 
             inverse square distance from the selected instance. } 
    \item{"DKM"}{Measure named after Dietterich, Kearns, and Mansour who proposed it in 1996. }
    \item{"ReliefFexpC"}{Cost-sensitive ReliefF algorithm with expected costs. }
    \item{"ReliefFavgC"}{Cost-sensitive ReliefF algorithm with average costs. }
    \item{"ReliefFpe"}{Cost-sensitive ReliefF algorithm with expected probability. }
    \item{"ReliefFpa"}{Cost-sensitive ReliefF algorithm with average probability. }
    \item{"ReliefFsmp"}{Cost-sensitive ReliefF algorithm with cost sensitive sampling. }
    \item{"GainRatioCost" }{Cost-sensitive variant of GainRatio. }
    \item{"DKMcost" }{Cost-sensitive variant of DKM. }
    \item{"ReliefKukar"}{Cost-sensitive Relief algorithm introduced by Kukar in 1999. }
    \item{"MDLsmp" }{Cost-sensitive variant of MDL where costs are introduced through sampling.}
    \item{"ImpurityEuclid"}{Euclidean distance as impurity function on within node class distributions.}
    \item{"ImpurityHellinger"}{Hellinger distance as impurity function on within node class distributions.}
    \item{"UniformDKM"}{Dietterich-Kearns-Mansour (DKM) with uniform priors. }
    \item{"UniformGini"}{Gini index with uniform priors.}                      
    \item{"UniformInf"}{Information gain with uniform priors.} 
    \item{"UniformAccuracy"}{Accuracy with uniform priors. }               
    \item{"EqualDKM"}{Dietterich-Kearns-Mansour (DKM) with equal weights for splits. }
    \item{"EqualGini"}{Gini index with equal weights for splits.}                      
    \item{"EqualInf"}{Information gain with equal weights for splits. }              
    \item{"EqualHellinger"}{Two equally weighted splits based Hellinger distance.} 
    \item{"DistHellinger"}{Hellinger distance between class distributions in branches.}
    \item{"DistAUC"}{AUC distance between splits.}               
    \item{"DistAngle"}{Cosine of angular distance between splits.}              
    \item{"DistEuclid"}{Euclidean distance between splits.}              
    }    
   For regression problem the implemented measures are:
   \describe{
    \item{"RReliefFequalK"}{RReliefF algorithm where k nearest instances have equal weight. }
    \item{"ReliefFexpRank"}{RReliefF algorithm where k nearest instances have weight exponentially decreasing with 
           increasing rank. Rank of nearest instance is determined by the increasing (Manhattan) distance from the selected instance.
           This is a default choice for methods taking conditional dependencies among the attributes into account. }
    \item{"RReliefFbestK"}{RReliefF algorithm where all possible k (representing k nearest instances)
     are tested and for each feature the highest score is returned. Nearest instances have equal weights. }
    \item{"RReliefFwithMSE"}{A combination of RReliefF and MSE algorithms. }
    \item{"MSEofMean"  }{Mean Squared Error as heuristic used to measure error by mean predicted value after split on the feature.}
    \item{"MSEofModel"  }{Mean Squared Error of an arbitrary model used on splits resulting from the feature. 
      The model is chosen with parameter \code{modelTypeReg}. }
    \item{"MAEofModel"  }{Mean Absolute Error of an arbitrary model used on splits resulting from the feature. 
      The model is chosen with parameter \code{modelTypeReg}. If we use median as the model, we get robust equivalent 
      to \code{MSEofMean}.}
    \item{"RReliefFdistance"}{RReliefF algorithm where k nearest instances are weighed  directly with its 
             inverse distance from the selected instance. Usually using ranks instead of distance
             as in \code{RReliefFexpRank} is more effective. }
    \item{"RReliefFsqrDistance"}{RReliefF algorithm where k nearest instances are weighed  with its 
             inverse square distance from the selected instance. } 
   }        
   
  There are some additional parameters \bold{\dots } available which are used by specific evaluation heuristics.
  Their list and short description is available by calling \code{\link{helpCore}}. See Section on attribute evaluation.
  
  The attributes can also be evaluated via random forest out-of-bag set with function \code{\link{rfAttrEval}}.
  
  Evaluation and visualization of ordered attributes is covered in function \code{\link{ordEval}}.  
}
\value{
  The method returns a vector of evaluations for the features in the order specified by the formula.
  In case of parameter \code{binaryEvaluateNumericAttributes=TRUE} the method returns a list with two components:
  \code{attrEval} and \code{splitPointNum}. The \code{attrEval} contains 
  a vector of evaluations for the features in the order specified by the formula. The \code{splitPointNum}
  contains the split points of numeric attributes which produced the given attribute evaluation scores.
}
\examples{
# use iris data

# run method ReliefF with exponential rank distance  
estReliefF <- attrEval(Species ~ ., iris, 
                       estimator="ReliefFexpRank", ReliefIterations=30)
print(estReliefF)

# alternatively and more appropriate for large data sets 
# one can specify just the target variable
# estReliefF <- attrEval("Species", iris, estimator="ReliefFexpRank",
#                        ReliefIterations=30)

# print all available estimators
infoCore(what="attrEval")
}
\references{ 
Marko Robnik-Sikonja, Igor Kononenko: Theoretical and Empirical Analysis of ReliefF and RReliefF.
    \emph{Machine Learning Journal}, 53:23-69, 2003
    
Marko Robnik-Sikonja: Experiments with Cost-sensitive Feature Evaluation. 
   In Lavrac et al.(eds): \emph{Machine Learning, Proceedings of ECML 2003}, Springer, Berlin, 2003, pp. 325-336
   
Igor Kononenko: On Biases in Estimating Multi-Valued Attributes.
  In \emph{Proceedings of the International Joint Conference on Artificial Intelligence (IJCAI'95)}, 
  pp. 1034-1040, 1995 

Some of these references are available also from \url{http://lkm.fri.uni-lj.si/rmarko/papers/}

 }
 
\author{ Marko Robnik-Sikonja }


\keyword{ regression }
\keyword{ nonlinear }
\keyword{ classif }

\seealso{
\code{\link{CORElearn}},
\code{\link{CoreModel}},
\code{\link{rfAttrEval}},
\code{\link{ordEval}},
\code{\link{helpCore}}, 
\code{\link{infoCore}}.
}

