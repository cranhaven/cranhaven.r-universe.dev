# Cutting criteria

#' @author Adan M. Rodriguez
#' @author Francisco Aragón Royón
#' @title Select K best
#' @description Generates a direct search function that takes the 'k' features with the greatest evaluations (The features evaluation is individual). This function is called internally within the \code{\link{directSearchAlgorithm}} function.
#' 
#' @param k Number (positive integer) of returned features
#'
#' @return Returns a direct search function that is used in the feature selection process.
#'
#' @export
#'
#' @examples
#' \dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly and performing a direct search process
#' ## Classification problem
#' 
#' 
#' # Generates the filter evaluation function
#' filter_evaluator <- filterEvaluator('determinationCoefficient')
#' 
#' # Generates the direct search function with k-best
#' skb_direct_search <- selectKBest()
#' # Performs the direct search process directly (parameters: dataset, target variable and evaluator)
#' skb_direct_search(iris, 'Species', filter_evaluator)
#' }
selectKBest <- function(k=1) {
  
  selectKBestDirectSearch <- function(data, class, featureEval) {
    # Take only the features of the data set
    column.names <- names(data)
    class.position <- which(column.names == class) 
    features <- data[-class.position] 
    
    # Check if the k value is valid
    if (k <= 0) {
      stop("k should be > 0")
    } else if (k > ncol(features)) {
      stop("k cannot be greater than the number of features")
    }
    
    # Check for maximization-minimization
    metricTarget <- attr(featureEval,'target')
    if(metricTarget=="maximize"){
      max <- TRUE
    }else if(metricTarget=="minimize"){
      max <- FALSE
    }else{ # Metric is not specified
      # Wrapper methods use by default RMSE for regression and Acuraccy for classification (in filter methods the metric is always specified)
      max <- ifelse(is.factor(data[,class]), TRUE, FALSE)
    }
    
    values <- NULL
    names <- NULL
    
    # Evaluate each feature separately
    for (i in colnames(features)) {
      names[length(names) + 1] <- i
      value <- featureEval(data, class, i)
      values[length(values) + 1] <- value
    }
    
    # Create a data.frame with the values
    feats <- data.frame(values)
    rownames(feats) <- names
    
    # Check for maximization-minimization
    if(max){ # Classification -> maximize
      # Sort the features
      sorted.features <- rownames(feats)[order(feats$values, decreasing = TRUE)] 
      sorted.values <- feats[order(feats$values, decreasing = TRUE),1]
    }else{ # Regression -> minimize
      # Sort the features
      sorted.features <- rownames(feats)[order(feats$values)]
      sorted.values <- feats[order(feats$values),1]
    }
    
    # List with results
    res <- list(NULL)
    res[[1]] <- matrix(rep(0,ncol(features)), nrow=1, byrow=FALSE, dimnames=list(c(),colnames(features)))
    res[[1]][match(sorted.features[1:k],colnames(features))] <- 1
    res[[2]] <- sorted.features[1:k] # Select features according to the k highest/lowest scores
    res[[3]] <- sorted.values[1:k] # Select features according to the k highest/lowest scores
    names(res) <- c("bestFeatures","featuresSelected","valuePerFeature") 
    
    return(res)
  }
  attr(selectKBestDirectSearch,'shortName') <- "selectKBest"
  attr(selectKBestDirectSearch,'name') <- "Select K Best"
  
  return(selectKBestDirectSearch)
}


#TODO: Reference
#' @author Adan M. Rodriguez
#' @author Francisco Aragón Royón
#' @title Select Percentile
#' @description Generates a direct search function that selects a fraction, given as a percentage, of the total number of available features (The features evaluation is individual). This function is called internally within the \code{\link{directSearchAlgorithm}} function.
#' 
#' @param percentile Number (positive integer) between 0 and 100
#'
#' @return Returns a direct search function that is used in the feature selection process.
#' 
#' @export
#'
#' @examples
#' \dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly and performing a direct search process
#' ## Classification problem
#' 
#' 
#' # Generates the filter evaluation function
#' filter_evaluator <- filterEvaluator('determinationCoefficient')
#' 
#' # Generates the direct search function with percentile
#' sp_direct_search <- selectPercentile()
#' # Performs the direct search process directly (parameters: dataset, target variable and evaluator)
#' sp_direct_search(iris, 'Species', filter_evaluator)
#' }
selectPercentile <- function(percentile=80) {
 
  selectPercentileDirectSearch <- function(data, class, featureEval) {
    
    # Take only the features of the data set
    column.names <- names(data) 
    class.position <- which(column.names == class) 
    features <- data[-class.position] 
    
    # Check if the percentile value is valid
    if (percentile < 0 || percentile > 100) {
      stop ("Percentile should be >=0 and <= 100")
    }
    
    # Check for maximization-minimization
    metricTarget <- attr(featureEval,'target')
    if(metricTarget=="maximize"){
      max <- TRUE
    }else if(metricTarget=="minimize"){
      max <- FALSE
    }else{ # Metric is not specified
      # Wrapper methods use by default RMSE for regression and Acuraccy for classification (in filter methods the metric is always specified)
      max <- ifelse(is.factor(data[,class]), TRUE, FALSE)
    }
    
    values <- NULL
    names <- NULL
    
    # Evaluate each feature separately
    for (i in colnames(features)) {
      names[length(names) + 1] <- i
      value <- featureEval(data, class, i)
      values[length(values) + 1] <- value
    }
    
    # Create a data.frame with the values
    feats <- data.frame(values)
    rownames(feats) <- names
    
    # Check for maximization-minimization
    if(max){ # Classification -> maximize
      # Sort the features
      sorted.features <- rownames(feats)[order(feats$values, decreasing = TRUE)] 
      sorted.values <- feats[order(feats$values, decreasing = TRUE),1]
    }else{ # Regression -> minimize
      # Sort the features
      sorted.features <- rownames(feats)[order(feats$values)]
      sorted.values <- feats[order(feats$values),1]
    }
    
    percentile <- percentile / 100
    max.features <- (length(sorted.features) * percentile)
    
    # List with results
    res <- list(NULL)
    res[[1]] <- matrix(rep(0,ncol(features)), nrow=1, byrow=FALSE, dimnames=list(c(),colnames(features)))
    res[[1]][match(sorted.features[1:round(max.features)],colnames(features))] <- 1
    res[[2]] <- sorted.features[1:round(max.features)] # Select features according to a percentile of the highest/lowest scores
    res[[3]] <- sorted.values[1:round(max.features)] # Select features according to a percentile of the highest/lowest scores
    names(res) <- c("bestFeatures","featuresSelected","valuePerFeature") 
    
    return(res)
  }
  attr(selectPercentileDirectSearch,'shortName') <- "selectPercentile"
  attr(selectPercentileDirectSearch,'name') <- "Select Percentile"

  return(selectPercentileDirectSearch)
}


#TODO: Reference
#' @author Adan M. Rodriguez
#' @author Francisco Aragón Royón
#' @title Select threshold
#' @description Generates a direct search function that selects the features whose evaluation is over/under a user given threshold (It depends on the method that generates the evaluation measure. For example: under for regression methods, over for classification methods, etc.)(The features evaluation is individual). Features that do not satisfy the threshold, will be removed. This function is called internally within the \code{\link{directSearchAlgorithm}} function.
#' 
#' @param threshold - Number between 0 and 1
#'
#' @return Returns a direct search function that is used in the feature selection process.
#' 
#' @export
#'
#' @examples
#' \dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly and performing a direct search process
#' ## Classification problem
#' 
#' 
#' # Generates the filter evaluation function
#' filter_evaluator <- filterEvaluator('determinationCoefficient')
#' 
#' # Generates the direct search function with threshold
#' st_direct_search <- selectThreshold()
#' # Performs the direct search process directly (parameters: dataset, target variable and evaluator)
#' st_direct_search(iris, 'Species', filter_evaluator) 
#' }
selectThreshold <- function(threshold=0.1) {
 
  selectThresholdDirectSearch <- function(data, class, featureEval) {
    # Take only the features of the data set
    column.names <- names(data) 
    class.position <- which(column.names == class)
    features <- data[-class.position] 
    
    # Check for maximization-minimization
    metricTarget <- attr(featureEval,'target')
    if(metricTarget=="maximize"){
      max <- TRUE
    }else if(metricTarget=="minimize"){
      max <- FALSE
    }else{ # Metric is not specified
      # Wrapper methods use by default RMSE for regression and Acuraccy for classification (in filter methods the metric is always specified)
      max <- ifelse(is.factor(data[,class]), TRUE, FALSE)
    }
    
    values <- NULL
    names <- NULL
    
    # Evaluate each feature separately
    for (i in colnames(features)) {
      names[length(names) + 1] <- i
      value <- featureEval(data, class, i)
      values[length(values) + 1] <- value
    }
    
    # Create a data.frame with the values
    feats <- data.frame(values)
    rownames(feats) <- names
    
    # Check for maximization-minimization
    if(max){ # Classification -> maximize
      # Select the features that exceed the threshold
      over.threshold <- NULL
      over.threshold.value <- NULL
      for (i in 1:nrow(feats)) {
        if (feats$values[i] > threshold) {
          row <- rownames(feats)[i]
          over.threshold <- c(over.threshold, row)
          over.threshold.value <- c(over.threshold.value, feats$values[i])
        }
      }    
      # Sort the features
      if(!is.null(over.threshold)){
        sorted.features <- over.threshold[order(over.threshold.value, decreasing = TRUE)]
        sorted.values <- over.threshold.value[order(over.threshold.value, decreasing = TRUE)]      
      }else{
        sorted.features <- NA
        sorted.values <- NA
      }
      
    }else{ # Regression -> minimize
      # Select the features that not exceed the threshold
      under.threshold <- NULL
      under.threshold.value <- NULL
      for (i in 1:nrow(feats)) {
        if (feats$values[i] < threshold) {
          row <- rownames(feats)[i]
          under.threshold <- c(under.threshold, row)
          under.threshold.value <- c(under.threshold.value, feats$values[i])
        }
      }    
      # Sort the features
      if(!is.null(under.threshold)){
        sorted.features <- under.threshold[order(under.threshold.value)]
        sorted.values <- under.threshold.value[order(under.threshold.value)]
      }else{
        sorted.features <- NA
        sorted.values <- NA
      }
    }
    
    
    # List with results
    res <- list(NULL)
    res[[1]] <- matrix(rep(0,ncol(features)), nrow = 1, byrow = FALSE, dimnames = list(c(),colnames(features)))
    res[[1]][match(sorted.features,colnames(features))] <- 1
    res[[2]] <- sorted.features
    res[[3]] <- sorted.values
    names(res) <- c("bestFeatures","featuresSelected","valuePerFeature") 
    
    return(res)
  }
  attr(selectThresholdDirectSearch,'shortName') <- "selectThreshold"
  attr(selectThresholdDirectSearch,'name') <- "Select Threshold"
  
  return(selectThresholdDirectSearch)
}


#TODO: Reference
# Evaluation over a threshold given as a fraction of the range of evaluation
#' @author Adan M. Rodriguez
#' @author Francisco Aragón Royón
#' @title Select threshold range
#' @description Generates a direct search function that selects the features whose evaluation is over a threshold, where this threshold is given as: (((min - max) * p.threshold) + max)(The features evaluation is individual). This function is called internally within the \code{\link{directSearchAlgorithm}} function.
#' 
#' @param p.threshold - Number between 0 and 1
#'
#' @return Returns a direct search function that is used in the feature selection process.
#' 
#' @export
#'
#' @examples
#' \dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly and performing a direct search process
#' ## Classification problem
#' 
#' 
#' # Generates the filter evaluation function
#' filter_evaluator <- filterEvaluator('determinationCoefficient')
#' 
#' # Generates the direct search function with threshold range
#' str_direct_search <- selectThresholdRange()
#' # Performs the direct search process directly (parameters: dataset, target variable and evaluator)
#' str_direct_search(iris, 'Species', filter_evaluator)
#' }
selectThresholdRange <- function(p.threshold=0.8) {

  selectThresholdRangeDirectSearch <- function(data, class, featureEval) {
    # Take only the features of the data set
    column.names <- names(data) 
    class.position <- which(column.names == class) 
    features <- data[-class.position] 
    
    # Check for maximization-minimization
    metricTarget <- attr(featureEval,'target')
    if(metricTarget=="maximize"){
      max <- TRUE
    }else if(metricTarget=="minimize"){
      max <- FALSE
    }else{ # Metric is not specified
      # Wrapper methods use by default RMSE for regression and Acuraccy for classification (in filter methods the metric is always specified)
      max <- ifelse(is.factor(data[,class]), TRUE, FALSE)
    }
    
    values <- NULL
    names <- NULL
    
    # Evaluate each feature separately
    for (i in colnames(features)) {
      names[length(names) + 1] <- i
      value <- featureEval(data, class, i)
      values[length(values) + 1] <- value
    }
    
    # Create a data.frame with the values
    feats <- data.frame(values)
    rownames(feats) <- names
    
    # Take the max and min value to evaluate the threshold
    max <- max(feats)
    min <- min(feats)
    threshold <- ((max - min) * p.threshold) + min
    
    # Check for maximization-minimization
    if(max){ # Classification -> maximize
      # Select the features that exceed the threshold
      over.threshold <- NULL
      over.threshold.value <- NULL
      for (i in 1:nrow(feats)) {
        if (feats$values[i] > threshold) {
          row <- rownames(feats)[i]
          over.threshold <- c(over.threshold, row)
          over.threshold.value <- c(over.threshold.value, feats$values[i])
        }
      }    
      # Sort the features
      if(!is.null(over.threshold)){
        sorted.features <- over.threshold[order(over.threshold.value, decreasing = TRUE)]
        sorted.values <- over.threshold.value[order(over.threshold.value, decreasing = TRUE)]      
      }else{
        sorted.features <- NA
        sorted.values <- NA
      }
      
    }else{ # Regression -> minimize
      # Select the features that not exceed the threshold
      under.threshold <- NULL
      under.threshold.value <- NULL
      for (i in 1:nrow(feats)) {
        if (feats$values[i] < threshold) {
          row <- rownames(feats)[i]
          under.threshold <- c(under.threshold, row)
          under.threshold.value <- c(under.threshold.value, feats$values[i])
        }
      }    
      # Sort the features
      if(!is.null(under.threshold)){
        sorted.features <- under.threshold[order(under.threshold.value)]
        sorted.values <- under.threshold.value[order(under.threshold.value)]
      }else{
        sorted.features <- NA
        sorted.values <- NA
      }
    }
    
    
    # List with results
    res <- list(NULL)
    res[[1]] <- matrix(rep(0,ncol(features)), nrow=1, byrow=FALSE, dimnames=list(c(),colnames(features)))
    res[[1]][match(sorted.features,colnames(features))] <- 1
    res[[2]] <- sorted.features
    res[[3]] <- sorted.values
    names(res) <- c("bestFeatures","featuresSelected","valuePerFeature") 
    
    return(res)
  }
  attr(selectThresholdRangeDirectSearch,'shortName') <- "selectThresholdRange"
  attr(selectThresholdRangeDirectSearch,'name') <- "Select Threshold Range"
  
  return(selectThresholdRangeDirectSearch)
}

#TODO: Reference
#' @author Adan M. Rodriguez
#' @author Francisco Aragón Royón
#' @title Select difference
#' @description Generates a direct search function that selects features (in descending order from the best evaluation measure to the lowest) until evaluation difference is over a threshold (The features evaluation is individual). This function is called internally within the \code{\link{directSearchAlgorithm}} function.
#' 
#' @param d.threshold - Number between 0 and 1, to calculate the slope
#'
#' @return Returns a direct search function that is used in the feature selection process.
#' 
#' @export
#'
#' @examples
#' \dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly and performing a direct search process
#' ## Classification problem
#' 
#' 
#' # Generates the filter evaluation function
#' filter_evaluator <- filterEvaluator('determinationCoefficient')
#' 
#' # Generates the direct search function with difference
#' sd_direct_search <- selectDifference()
#' # Performs the direct search process directly (parameters: dataset, target variable and evaluator)
#' sd_direct_search(iris, 'Species', filter_evaluator)
#' }
selectDifference <- function(d.threshold=0.2) {
  
  selectDifferenceDirectSearch <- function(data, class, featureEval) {
  
    # Take only the features of the data set
    column.names <- names(data) 
    class.position <- which(column.names == class) 
    features <- data[-class.position] 
    
    # Check for maximization-minimization
    metricTarget <- attr(featureEval,'target')
    if(metricTarget=="maximize"){
      max <- TRUE
    }else if(metricTarget=="minimize"){
      max <- FALSE
    }else{ # Metric is not specified
      # Wrapper methods use by default RMSE for regression and Acuraccy for classification (in filter methods the metric is always specified)
      max <- ifelse(is.factor(data[,class]), TRUE, FALSE)
    }
    
    values <- NULL
    names <- NULL
    
    # Evaluate each feature separately
    for (i in colnames(features)) {
      names[length(names) + 1] <- i
      value <- featureEval(data, class, i)
      values[length(values) + 1] <- value
    }
    
    # Create a data.frame with the values
    feats <- data.frame(values)
    rownames(feats) <- names
    
    # Check for maximization-minimization
    if(max){ # Classification -> maximize
      # Sort the features in descending order
      sorted.features <- rownames(feats)[order(-feats$values)] 
      sorted.values <- feats$values[order(-feats$values)]
      sorted.frame <- data.frame(row.names = sorted.features, values = sorted.values)
      difference <- 0
      num.features <- nrow(sorted.frame)
      under.threshold <- NULL
      under.value <- NULL
      
      # Always select the first feature
      row <- (rownames(sorted.frame)[1])
      under.threshold <- c(under.threshold, row)
      under.value <- c(under.value, sorted.frame[row,1])
      
      # Select the features whose difference does not exceed the threshold
      for (i in 1:num.features) {
        if (i == num.features)
          break
        else {
          # Calculate the difference between 2 features
          first <- sorted.frame$values[i]
          second <- sorted.frame$values[i+1]
          difference <- abs(first - second)
          
          # Select features that meet the criteria. Stop the algorithm when criterion is not met
          if (difference < d.threshold) {
            row <- rownames(sorted.frame)[i+1]
            under.threshold <- c(under.threshold, row) 
            under.value <- c(under.value, sorted.frame[row,1])
          } else { 
            break
          }
        }
      }
      
    }else{ # Regression -> minimize
      # Sort the features in ascending order
      sorted.features <- rownames(feats)[order(feats$values)] 
      sorted.values <- feats$values[order(feats$values)]
      sorted.frame <- data.frame(row.names = sorted.features, values = sorted.values)
      difference <- 0
      num.features <- nrow(sorted.frame)
      under.threshold <- NULL
      under.value <- NULL
      
      # Always select the first feature
      row <- (rownames(sorted.frame)[1])
      under.threshold <- c(under.threshold, row)
      under.value <- c(under.value, sorted.frame[row,1])
      
      # Select the features whose difference does not exceed the threshold
      for (i in 1:num.features) {
        if (i == num.features)
          break
        else {
          # Calculate the difference between 2 features
          first <- sorted.frame$values[i]
          second <- sorted.frame$values[i+1]
          difference <- abs(first - second)
          
          # Select features that meet the criteria. Stop the algorithm when criterion is not met
          if (difference < d.threshold) {
            row <- rownames(sorted.frame)[i+1]
            under.threshold <- c(under.threshold, row) 
            under.value <- c(under.value, sorted.frame[row,1])
          } else { 
            break
          }
        }
      }
      
    }
    
    
    # List with results
    res <- list(NULL)
    res[[1]] <- matrix(rep(0,ncol(features)), nrow=1, byrow=FALSE, dimnames=list(c(),colnames(features)))
    res[[1]][match(under.threshold,colnames(features))] <- 1
    res[[2]] <- under.threshold
    res[[3]] <- under.value
    names(res) <- c("bestFeatures","featuresSelected","valuePerFeature") 
    
    return(res)
  }
  attr(selectDifferenceDirectSearch,'shortName') <- "selectDifference"
  attr(selectDifferenceDirectSearch,'name') <- "Select Difference"
  
  return(selectDifferenceDirectSearch)
}


#TODO: Reference
#' @author Adan M. Rodriguez
#' @title Select slope
#' @description Generates a direct search function that selects features (in descending order from the best evaluation measure to the lowest) until the slope to the next feature is over a threshold (The features evaluation is individual). The slope is calculated as: (s.threshold) / (number of features). This function is called internally within the \code{\link{directSearchAlgorithm}} function.
#' 
#' @param s.threshold - Number between 0 and 1
#'
#' @return Returns a direct search function that is used in the feature selection process.
#' 
#' @export
#'
#' @examples
#' \dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly and performing a direct search process
#' ## Classification problem
#' 
#' 
#' # Generates the filter evaluation function
#' filter_evaluator <- filterEvaluator('determinationCoefficient')
#' 
#' # Generates the direct search function with slope
#' ss_direct_search <- selectSlope()
#' # Performs the direct search process directly (parameters: dataset, target variable and evaluator)
#' ss_direct_search(iris, 'Species', filter_evaluator)
#' }
selectSlope <- function(s.threshold=1.5) {

  selectSlopeDirectSearch <- function(data, class, featureEval) {
    
    column.names <- names(data) 
    class.position <- which(column.names == class) 
    features <- data[-class.position] 
    n <- ncol(features)
    
    # Calculate the slope
    s.threshold <- s.threshold / n
    
    # Select features until the slope to the next feature is over a threshold
    selectDifference(s.threshold)(data, class, featureEval)
  }
  attr(selectSlopeDirectSearch,'shortName') <- "selectSlope"
  attr(selectSlopeDirectSearch,'name') <- "Select Slope"
  
  return(selectSlopeDirectSearch)

}
