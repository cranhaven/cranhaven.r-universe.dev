\name{ordEval}
\alias{ordEval}
\alias{OrdEval}
\title{ Evaluation of ordered attributes }
\description{
  The method evaluates the quality of ordered attributes
   specified by the formula with ordEval algorithm.
}
\usage{
ordEval(formula, data, file=NULL, rndFile=NULL, 
        variant=c("allNear","attrDist1","classDist1"), ...)
}
\arguments{
  \item{formula}{ Either a formula specifying the attributes to be evaluated and the target variable, or a name of target variable, or an index of target variable. }
  \item{data}{ Data frame with evaluation data. }
  \item{file}{ Name of file where evaluation results will be written to. }
  \item{rndFile}{ Name of file where evaluation of random normalizing attributes will be written to. }
  \item{variant}{ Name of the variant of ordEval algorithm. Can be any of \code{"allNear", "attrDist1"}, or \code{"classDist1"}. }
  \item{\dots }{ Other options specific to ordEval or common to other context-sensitive evaluation methods (e.g., ReliefF). }
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
 
In the data frame \code{data} take care to supply the ordinal data as factors and to provide equal levels for them
  (this is not necessary what one gets with \code{\link{read.table}}).
  See example below.
  
  The output can be optionally written to files \code{file} and \code{rndFile},
  in a format used by visualization methods in \code{\link{plotOrdEval}}.
   
  The variant of the algorithm actually used is controlled with \code{variant} parameter
  which can have values "allNear", "attrDist1", and "classDist1". The default value
  is "allNear" which takes all nearest neighbors into account in evaluation of attributes.
  Variant "attrDist1" takes only  neighbors with attribute value at most 1 different from
  current case into account (for each attribute separately). This makes sense when we want to 
  see the thresholds of reinforcement, and therefore observe just small change up or down
  (it makes sense to combine this with \code{equalUpDown=TRUE} in \code{\link{plot.ordEval}} function).   
  The "classDist1" variant takes only  neighbors with class value at most 1 different from
  current case into account. This makes sense if we want to observe strictly small
  changes in upward/downward reinforcement and has little effect in practical applications.
  
  There are some additional parameters (note \bold{\dots }) some of which are common with other context-sensitive evaluation methods (e.g., ReliefF). 
  Their list of common parameters is available in \code{\link{helpCore}} (see subsection on attribute evaluation therein).
  The parameters specific to \code{\link{ordEval}} are: 
  \describe{
    \item{ordEvalNoRandomNormalizers}{type: integer, default value: 0, value range: 0, Inf, \cr
       number of randomly shuffled attributes for normalization of each attribute (0=no normalization). This parameter should be set to 
       a reasonably high value (e.g., 200) in order to produce reliable confidence intervals with \code{\link{plot.ordEval}}. The parameters 
       \code{ordEvalBootstrapNormalize} and \code{ordEvalNormalizingPercentile} only make sense if this parameter is larger than 0.}
    \item{ordEvalBootstrapNormalize}{type: logical, default value: FALSE \cr
       are features used for normalization constructed with bootstrap sampling or random permutation. }
    \item{ordEvalNormalizingPercentile}{type: numeric, default value: 0.025, value range: 0, 0.5 \cr
       percentile defines the length of confidence interval obtained with random normalization. Percentile \code{t} forms
       interval by taking the \eqn{n\cdot t}{nt} and \eqn{n(1-t)} random evaluation as the confidence interval boundaries, thereby forming 
       \eqn{100(1-2t)}\%  confidence interval (\code{t}=0.025 gives 95\% confidence interval). The value \eqn{n} is set by 
       \code{ordEvalNoRandomNormalizers} parameter.}
    \item{attrWeights}{type: character, \cr
       a character vector representing a list of attribute weights in the ordEval distance measure. }
}
  
 Evaluation of attributes without specifics of ordered attributes is covered in function \code{\link{attrEval}}.  
}
\value{
The method returns a list with following components:
\item{reinfPosAV}{a matrix of positive reinforcement for attributes' values, }
\item{reinfNegAV}{a matrix of negative reinforcement for attributes' values, }
\item{anchorAV}{a matrix of anchoring for attributes' values, }
\item{noAV}{a matrix containing count for each value of each attribute, }
\item{reinfPosAttr}{a vector of positive reinforcement for attributes, }
\item{reinfNegAttr}{a matrix of negative reinforcement for attributes, }
\item{anchorAttr}{a matrix of anchoring for attributes, }
\item{noAVattr}{a vector containing count of valid values of each attribute, }
\item{rndReinfPosAV}{a three dimensional array of statistics for random normalizing attributes'  positive reinforcement for attributes' values, }
\item{rndReinfPosAV}{a three dimensional array of statistics for random normalizing attributes' negative reinforcement for attributes' values, }
\item{rndAnchorAV}{a three dimensional array of statistics for random normalizing attributes' anchoring for attributes' values, }
\item{rndReinfPosAttr}{a three dimensional array of statistics for random normalizing attributes'  positive reinforcement for attributes, }
\item{rndReinfPosAttr}{a three dimensional array of statistics for random normalizing attributes' negative reinforcement for attributes, }
\item{rndAnchorAttr}{a three dimensional array of statistics for random normalizing attributes' anchoring for attributes. }
\item{attrNames}{the names of attributes}
\item{valueNames}{the values of attributes}
\item{noAttr}{number of attributes}
\item{ordVal}{maximal number of attribute values}
\item{variant}{the variant of the algorithm used}
\item{file}{the file to store the results}
\item{rndFile}{the file to store random normalizations}

The statistics used are median, 1st quartile, 3rd quartile, low and high percentile selected by \cr
\code{ordEvalNormalizingPercentile}, mean, standard deviation, and expected probability according to value distribution.  
With these statistics we can visualize significance of reinforcements using adapted box and whiskers plot.  
}
\examples{
#prepare a data set
dat <- ordDataGen(200)

# evaluate ordered features with ordEval
est <- ordEval(class ~ ., dat, ordEvalNoRandomNormalizers=100)
# print(est)
printOrdEval(est)  
plot(est)

}
\references{ 
Marko Robnik-Sikonja, Koen Vanhoof: Evaluation of ordinal attributes at value level. 
     \emph{Knowledge Discovery and Data Mining}, 14:225-243, 2007    

Marko Robnik-Sikonja, Igor Kononenko: Theoretical and Empirical Analysis of ReliefF and RReliefF.
    \emph{Machine Learning Journal}, 53:23-69, 2003
    
Some of the references are available also from \url{http://lkm.fri.uni-lj.si/rmarko/papers/}

 }
 
\author{ Marko Robnik-Sikonja }


\keyword{ regression }
\keyword{ nonlinear }
\keyword{ classif }

\seealso{
\code{\link{plot.ordEval}},
\code{\link{CORElearn}},
\code{\link{CoreModel}},
\code{\link{helpCore}}, 
\code{\link{infoCore}}.
}

