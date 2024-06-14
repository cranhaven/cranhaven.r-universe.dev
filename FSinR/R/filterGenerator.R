# Filter evaluator generator

#' @author Francisco Aragón Royón
#' @title Filter measure generator
#' @description Generates a filter function to be used as an evaluator in the feature selection proccess. More specifically, the result of calling this function is another function that is passed on as a parameter to the \code{\link{featureSelection}} function. However, you can also run this function directly to generate an evaluation measure.
#' @param filter Name of the filter method. The available filter methods are:
#' \describe{
#'   \item{binaryConsistency}{Binary consistency measure. See \code{\link{binaryConsistency}} }
#'   \item{chiSquared}{Chi squared measure. See \code{\link{chiSquared}} }
#'   \item{cramer}{Cramer V measure. See \code{\link{cramer}} }
#'   \item{determinationCoefficient}{R Squared, to continous features. See \code{\link{determinationCoefficient}} }
#'   \item{fscore}{F-score measure. See \code{\link{fscore}} }
#'   \item{gainRatio}{The gain ratio measure. See \code{\link{gainRatio}} }
#'   \item{giniIndex}{Gini index measure. See \code{\link{giniIndex}} }
#'   \item{IEConsistency}{Inconsistent Examples consistency measure. See \code{\link{IEConsistency}} }
#'   \item{IEPConsistency}{Inconsistent Examples Pairs consistency measure. See \code{\link{chiSquared}} }
#'   \item{Jd}{Jd evaluation measure. See \code{\link{Jd}} }
#'   \item{MDLC}{MDLC evaluation measure. See \code{\link{MDLC}} }
#'   \item{mutualInformation}{The mutual information measure. See \code{\link{mutualInformation}} }
#'   \item{roughsetConsistency}{Rough Set consistency measure. See \code{\link{roughsetConsistency}} }
#'   \item{relief}{Relief. See \code{\link{relief}} }
#'   \item{ReliefFeatureSetMeasure}{Relief Feature Set Measure evaluation measure. See \code{\link{ReliefFeatureSetMeasure}} }
#'   \item{symmetricalUncertain}{Symmetrical uncertain measure. See \code{\link{symmetricalUncertain}} }
#'   
#' }
#' @param params List with the parameters of each filter method. For more details see each method. Default: empty list.
#' 
#' @return Returns a filter method that is used to generate an evaluation measure.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## Examples of a filter evaluator generation
#' 
#' filter_evaluator_1 <- filterEvaluator('cramer')
#' filter_evaluator_2 <- filterEvaluator('gainRatio')
#' filter_evaluator_3 <- filterEvaluator('MDLC')
#' 
#' 
#' ## Examples of a filter evaluator generation (with parameters)
#' 
#' filter_evaluator_1 <- filterEvaluator('relief', list(neighbours.count=4, sample.size=15))
#' filter_evaluator_2 <- filterEvaluator('ReliefFeatureSetMeasure', list(iterations = 10))
#' 
#' 
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly to evaluate a set of features
#' ## Classification problem
#' 
#' # Generates the filter evaluation function
#' filter_evaluator <- filterEvaluator('ReliefFeatureSetMeasure')
#' # Evaluates features directly (parameters: dataset, target variable and features)
#' filter_evaluator(iris,'Species',c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'))
#' }
filterEvaluator <- function(filter, params=list()){
  
  # Check the parameter
  if(!is.list(params)){
    stop('The filter method parameters are not in a list.')
  }
  
  # List of supported filter methods
  algorithms <- list('binaryConsistency', 'chiSquared', 'cramed', 'determinationCoefficient', 'fscore', 'gainRatio', 'giniIndex', 'IEConsistency', 'IEPConsistency', 'Jd', 'MDLC', 'mutualInformation', 'roughsetConsistency', 'relief', 'ReliefFeatureSetMeasure', 'symmetricalUncertain')
  
  switch(filter, 
         'binaryConsistency'={
           do.call(binaryConsistency,params)
         },
         'chiSquared'={
           do.call(chiSquared,params)
         },
         'cramer'={
           do.call(cramer,params)
         },
         'determinationCoefficient'={
           do.call(determinationCoefficient,params)
         },
         'gainRatio'={
           do.call(gainRatio,params)
         },
         'giniIndex'={
           do.call(giniIndex,params)
         },
         'IEConsistency'={
           do.call(IEConsistency,params)
         },
         'IEPConsistency'={
           do.call(IEPConsistency,params)
         },
         'Jd'={
           do.call(Jd,params)
         },
         'MDLC'={
           do.call(MDLC,params)
         },
         'mutualInformation'={
           do.call(mutualInformation,params)
         },
         'roughsetConsistency'={
           do.call(roughsetConsistency,params)
         },
         'relief'={
           do.call(relief,params)
         },
         'ReliefFeatureSetMeasure'={
           do.call(ReliefFeatureSetMeasure,params)
         },
         'symmetricalUncertain'={
           do.call(symmetricalUncertain,params)
         },
         {
           stop('The filter method passed as a parameter is not supported by the package.')
         }
  )

}
attr(filterEvaluator,'shortName') <- "filterGenerator"
attr(filterEvaluator,'name') <- "Filter Measure Generator"
attr(filterEvaluator,'methods') <- c('binaryConsistency', 'chiSquared', 'cramer', 'determinationCoefficient', 'fscore', 'gainRatio', 'giniIndex', 'IEConsistency', 'IEPConsistency', 'Jd', 'MDLC', 'mutualInformation', 'roughsetConsistency', 'relief', 'RFSM', 'symmetricalUncertain')