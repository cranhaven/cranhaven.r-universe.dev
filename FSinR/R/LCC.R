#' @importFrom purrr map_dbl
orderFeatures <- function(data, class, features, measure) {
  values <- features %>% map_dbl(function(x) measure(data, class ,x))
  result <- data.frame(feature = features, value = values)
  # Check for maximization-minimization
  metricTarget <- attr(measure,'target')
  if (metricTarget == "maximize") {
    max <- TRUE
  } else if (metricTarget == "minimize") {
    max <- FALSE
  } else {# Metric is not specified
    # Wrapper methods use by default RMSE for regression and Acuraccy for classification (in filter methods the metric is always specified)
    max <- ifelse(is.factor(data[,class]), TRUE, FALSE)
  }
  if (max) {
    return(as.vector(result[order(-result$value),c(1)]))
  } else {
    return(as.vector(result[order(result$value),c(1)]))
  }
}

#' @author Alfonso Jiménez-Vílchez
#' @title Linear Consistency-Constrained algorithm
#' @description Generates a hybrid search function based on Linear Consistency-Constrained algorithm described in \insertCite{ShinXu2009}{FSinR}. The algorithm combines two evaluation measures, the first evaluates each feature individually, and the second measure evaluate feature sets.
#' 
#' @param threshold Threshold
#'
#' @return Returns a hybrid search function that is used to guide the feature selection process.
#' 
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#' \dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly and performing a hybrid search process in a feature space
#' ## Classification problem
#' 
#' # Generates the first filter evaluation function (individual or set measure)
#' filter_evaluator_1 <- filterEvaluator('determinationCoefficient')
#' # Generates the second filter evaluation function (mandatory set measure)
#' filter_evaluator_2 <- filterEvaluator('ReliefFeatureSetMeasure')
#' 
#'   
#' # Generates the hybrid search function with LCC
#' LCC_hybrid_search <- LCC()
#' # Run the search process directly (params: dataset, target variable, evaluator1 & evaluator2)
#' LCC_hybrid_search(iris, 'Species', filter_evaluator_1, filter_evaluator_2)
#' }
LCC <- function(threshold = 0.9) {
  
  LCCHybridSearch <- function(data, class, featureEval=symmetricalUncertain(), featureSetEval=determinationCoefficient()) {
    
    if (attr(featureSetEval, 'kind') == "Individual measure") {
      stop('Only feature set measures can be used');
    }
    column.names <- names(data) 
    class.position <- which(column.names == class) 
    features <- column.names[-class.position] 
    feat.sub <- as.vector(features)
    
    best.value <- featureSetEval(data,class, features)
    
    # Check for maximization-minimization
    metricTarget <- attr(featureSetEval,'target')
    if (metricTarget == "maximize") {
      max <- TRUE
    } else if (metricTarget == "minimize") {
      max <- FALSE
    } else {# Metric is not specified
      # Wrapper methods use by default RMSE for regression and Acuraccy for classification (in filter methods the metric is always specified)
      max <- ifelse(is.factor(data[,class]), TRUE, FALSE)
    }
    
    if ((max && best.value >= threshold) || (!max && best.value <= threshold)) {
      feat.sub <- orderFeatures(data, class, feat.sub, featureEval)
      for (i in rev(seq(along = feat.sub))) {
        if (length(feat.sub) > 1) {
          feat <- feat.sub[[i]]
          feat.prueba <- feat.sub
          feat.prueba <- feat.prueba[feat.prueba != feat]
          value <- featureSetEval(data, class, feat.prueba)
          
          # Find the feature that removing it, we can get a better evaluation
          if ((max && value >= threshold) || (!max && value <= threshold)) {
            best.value <- value
            feat.sub <- feat.prueba
          }
        }

      }
    }
    
    res <- list(NULL)
    best.set.aux <- matrix(rep(0,(ncol(data)-1)), ncol=(ncol(data)-1), byrow=FALSE, dimnames=list(c(),column.names[-class.position]))
    best.set.aux[which(column.names[-class.position]%in%feat.sub)] <- 1
    res[[1]] <- best.set.aux
    res[[2]] <- best.value
    names(res) <- c("bestFeatures","bestFitness") 
    
    res
  }
  attr(LCCHybridSearch,'shortName') <- "LCC"
  attr(LCCHybridSearch,'name') <- "Linear Consistency-Constrained"
  
  return(LCCHybridSearch)
}

