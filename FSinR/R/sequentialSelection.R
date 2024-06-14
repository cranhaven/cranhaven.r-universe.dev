# Sequential Searchs
#' @author Adan M. Rodriguez
#' @author Alfonso Jiménez-Vílchez
#' @author Francisco Aragón Royón
#' @title Sequential Forward Selection
#' @description Generates a search function based on sequential forward selection. This function is called internally within the \code{\link{searchAlgorithm}} function. The SFS method \insertCite{Whitney1971}{FSinR} starts with an empty set of features and add a single feature at each step with a view to improving the evaluation of the set.
#' 
#' @param stopCriterion Define a maximum number of iterations. Disabled if the value is -1 (default: -1 )
#' @param stop If true, the function stops if next iteration does not improve current results (default: FALSE)
#'
#' @return Returns a search function that is used to guide the feature selection process.
#' 
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly and performing a search process in a feature space
#' ## Classification problem
#' 
#' # Generates the filter evaluation function with sfs
#' filter_evaluator <- filterEvaluator('determinationCoefficient')
#' 
#' # Generates the search function
#' sfs_search <- sequentialForwardSelection()
#' # Performs the search process directly (parameters: dataset, target variable and evaluator)
#' sfs_search(iris, 'Species', filter_evaluator)
#' }
sequentialForwardSelection <- function(stopCriterion=-1, stop=FALSE) {

  sequentialForwardSelectionSearch <- function(data, class, featureSetEval) {
    
    if (attr(featureSetEval, 'kind') == "Individual measure") {
      stop('Only feature set measures can be used');
    }
    # Extract and eliminate the class to have only the features in the variable 'features'
    column.names <- names(data) 
    class.position <- which(column.names == class) 
    features <- column.names[-class.position]
    
    if (stopCriterion != -1) {
      maxIterations <- min(length(features), stopCriterion)
    } else {
      maxIterations <- length(features)
    }
    
    # Check for maximization-minimization
    metricTarget <- attr(featureSetEval,'target')
    if(metricTarget=="maximize"){
      max <- TRUE
    }else if(metricTarget=="minimize"){
      max <- FALSE
    }else{ # Metric is not specified
      # Wrapper methods use by default RMSE for regression and Acuraccy for classification (in filter methods the metric is always specified)
      max <- ifelse(is.factor(data[,class]), TRUE, FALSE)
    }
    
    
    # In feat.sub we store the features that are part of the solution
    feat.sub <- c()
    if(max){ # Classification -> Maximize
      value.max <- 0
    }else{ # Regression -> Minimize
      value.max <- Inf
    }
    feat.sub.best <- NULL
    
    # For each feature...
    for (i in seq(maxIterations)) {
      
      # Find the best feature (only 1) to be included in this step
      best.feat <- NULL
      best.feat.value <- NULL
      
      # Try to include a feature in the best.feat array
      
      for (j in seq(along = features)) {
        feat <- features[j] 
        
        # Find the best feature to include in the result set of features
        if (! feat %in% feat.sub) {
          value <- featureSetEval(data, class, c(feat.sub, feat)) 
          
          if(max){ # Classification -> Maximize
            if (is.null(best.feat.value) || value > best.feat.value) {
              best.feat.value <- value
              best.feat <- feat
            }
          }else{ # Regression -> Minimize
            if (is.null(best.feat.value) || value < best.feat.value) {
              best.feat.value <- value
              best.feat <- feat
            }
          }
          
        }
      }
      
      # Always save the best evaluation
      if (length(feat.sub.best) > 0)
        value.max <- featureSetEval(data, class, feat.sub.best)
      
      if(is.factor(data[,class])){ # Classification -> Maximize
        if(best.feat.value <= value.max && stop) {
          break
        }
      }else{ # Regression -> Minimize
        if(best.feat.value >= value.max && stop) {
          break
        }
      }
      
      feat.sub[length(feat.sub) + 1] <- best.feat 
      # Remove the included feature, to not evalute it again
      features <- features[features != best.feat]
      
      # If including the new feature, we have a better set of features, we include it
      if(max){ # Classification -> Maximize
        if(best.feat.value > value.max) {
          feat.sub.best <- feat.sub
        }
      }else{ # Regression -> Minimize
        if(best.feat.value < value.max) {
          feat.sub.best <- feat.sub
        }
      }
      
    }
    
    # List with results
    res <- list(NULL)
    best.set.aux <- matrix(rep(0,(ncol(data)-1)), ncol=(ncol(data)-1), byrow=FALSE, dimnames=list(c(),column.names[-class.position]))
    best.set.aux[which(column.names[-class.position]%in%feat.sub.best)] <- 1
    res[[1]] <- best.set.aux
    res[[2]] <- value.max
    names(res) <- c("bestFeatures","bestFitness") 
    
    res
  }
  attr(sequentialForwardSelectionSearch,'shortName') <- "sfs"
  attr(sequentialForwardSelectionSearch,'name') <- "Sequential Forward Selection"
    
  return(sequentialForwardSelectionSearch)
}


#' @author Adan M. Rodriguez
#' @author Francisco Aragón Royón
#' @title Sequential Floating Forward Selection
#' @description Generates a search function based on sequential floating forward selection. This function is called internally within the \code{\link{searchAlgorithm}} function. The sffs method \insertCite{Pudil1994}{FSinR} starts with an empty set of features and add a single feature at each step with a view to improving the evaluation of the set. In addition, it checks whether removing any of the included features, improve the value of the set.
#'
#' @return Returns a search function that is used to guide the feature selection process.
#' 
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly and performing a search process in a feature space
#' ## Classification problem
#' 
#' # Generates the filter evaluation function
#' filter_evaluator <- filterEvaluator('determinationCoefficient')
#' 
#' # Generates the search function with sffs
#' sffs_search <- sequentialFloatingForwardSelection()
#' # Performs the search process directly (parameters: dataset, target variable and evaluator)
#' sffs_search(iris, 'Species', filter_evaluator)
#' }
sequentialFloatingForwardSelection <- function(){
  
  sequentialFloatingForwardSelectionSearch <- function(data, class, featureSetEval) {
    
    if (attr(featureSetEval, 'kind') == "Individual measure") {
      stop('Only feature set measures can be used');
    }
    # Extract and eliminate the class to have only the features in the variable 'features'
    column.names <- names(data) 
    class.position <- which(column.names == class) 
    features <- column.names[-class.position] 
    feat.sub <- c()
    
    # Check for maximization-minimization
    metricTarget <- attr(featureSetEval,'target')
    if(metricTarget=="maximize"){
      max <- TRUE
    }else if(metricTarget=="minimize"){
      max <- FALSE
    }else{ # Metric is not specified
      # Wrapper methods use by default RMSE for regression and Acuraccy for classification (in filter methods the metric is always specified)
      max <- ifelse(is.factor(data[,class]), TRUE, FALSE)
    }
    
    
    if(max){ # Classification -> Maximize
      value.max <- 0
    }else{ # Regression -> Minimize
      value.max <- Inf
    }
    
    
    # Step 1 (Inclusion): Find the best feature (only 1) to be included in this step
    for (i in seq(along = features)) {
      best.feat <- NULL
      best.feat.value <- NULL
      
      # Try to include a feature in the best.feat array
      for (j in seq(along = features)) {
        feat <- features[j]
        
        # Find the best feature to include in the result set of features
        if (! feat %in% feat.sub) {
          value <- featureSetEval(data, class, c(feat.sub, feat)) 
          
          if(max){ # Classification -> Maximize
            if (is.null(best.feat.value) || value > best.feat.value) {
              best.feat.value <- value
              best.feat <- feat
            }
          }else{ # Regression -> Minimize
            if (is.null(best.feat.value) || value < best.feat.value) {
              best.feat.value <- value
              best.feat <- feat
            }
          }
          
        }
      }
      
      # Always save the best evaluation
      if (length(feat.sub) > 0)
        value.max <- featureSetEval(data, class, feat.sub)
      
      # If including the new feature, we have a better set of features, we include it
      if(max){ # Classification -> maximize
        best <- best.feat.value > value.max
      }else{ # Regression -> minimize
        best <- best.feat.value < value.max
      }
      
      if(best) {
        feat.sub[length(feat.sub) + 1] <- best.feat 
        features <- features[features != best.feat]
        
        # Step 2: Conditional exclusion. Now, if removing a feature, we get a better set of features. We remove it
        if(length(feat.sub) > 1) {
          crit.func.max <- featureSetEval(data, class, feat.sub) 
          continue <- TRUE
          
          # We can exclude 1 or more features in each step
          while (continue == TRUE) {
            worst.feat.value <- FALSE
            for (i in seq(along=feat.sub)) {
              feat <- feat.sub[i] 
              feat.prueba <- feat.sub
              feat.prueba <- feat.prueba[feat.prueba != feat]
              crit.func.eval <- featureSetEval(data, class, feat.prueba)
              
              # Check if removing the feature, we get a better or even evaluation
              if(max){ # Classification -> maximize
                best.critic <- crit.func.eval >= crit.func.max
              }else{ # Regression -> minimize
                best.critic <- crit.func.eval <= crit.func.max
              }
              
              if (best.critic) {
                worst.feat <- feat
                crit.func.max <- crit.func.eval
                worst.feat.value <- TRUE
                # Do not remove the feature that was just included
                if(worst.feat == best.feat) 
                  worst.feat.value <- FALSE
              }
            }
            # Remove the feature 
            if (worst.feat.value == TRUE) {
              feat.sub <- feat.sub[feat.sub != worst.feat]
              features[length(features) + 1] <- worst.feat
            } else {
              continue <- FALSE
            }
          }
        }
      }
      
    }
    
    # List with results
    res <- list(NULL)
    best.set.aux <- matrix(rep(0,(ncol(data)-1)), ncol=(ncol(data)-1), byrow=FALSE, dimnames=list(c(),column.names[-class.position]))
    best.set.aux[which(column.names[-class.position]%in%feat.sub)] <- 1
    res[[1]] <- best.set.aux
    res[[2]] <- featureSetEval(data, class, feat.sub)
    names(res) <- c("bestFeatures","bestFitness") 
    
    res
  }
  attr(sequentialFloatingForwardSelectionSearch,'shortName') <- "sffs"
  attr(sequentialFloatingForwardSelectionSearch,'name') <- "Sequential Floating Forward Selection"
  
  return(sequentialFloatingForwardSelectionSearch)
}



#' @author Adan M. Rodriguez
#' @author Alfonso Jiménez-Vílchez
#' @author Francisco Aragón Royón
#' @title Sequential Backward Selection
#' @description Generates a search function based on sequential backward selection. This function is called internally within the \code{\link{searchAlgorithm}} function. The SBS method \insertCite{MarillGreen1963}{FSinR} starts with all the features and removes a single feature at each step with a view to improving the evaluation of the set. 
#' 
#' @param stopCriterion Define a maximum number of iterations. Disabled if the value is -1 (default: -1 )
#' @param stop If true, the function stops if next iteration does not improve current results (default: FALSE)
#'
#' @return Returns a search function that is used to guide the feature selection process.
#' 
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly and performing a search process in a feature space
#' ## Classification problem
#' 
#' # Generates the filter evaluation function with sbs
#' filter_evaluator <- filterEvaluator('determinationCoefficient')
#' 
#' # Generates the search function
#' sbs_search <- sequentialBackwardSelection()
#' # Performs the search process directly (parameters: dataset, target variable and evaluator)
#' sbs_search(iris, 'Species', filter_evaluator)
#' }
sequentialBackwardSelection <- function(stopCriterion=-1, stop=FALSE) {

  sequentialBackwardSelectionSearch <- function(data, class, featureSetEval){
    if (attr(featureSetEval, 'kind') == "Individual measure") {
      stop('Only feature set measures can be used');
    }
    # Extract and eliminate the class to have only the features in the variable 'features'
    column.names <- names(data) 
    class.position <- which(column.names == class) 
    features <- column.names[-class.position] 
    feat.sub <- as.vector(features)
    excluded.features <- NULL
    
    if (stopCriterion != -1) {
      maxIterations <- min(length(features)-1, stopCriterion)
    } else {
      maxIterations <- length(features)-1
    }
    
    # Check for maximization-minimization
    metricTarget <- attr(featureSetEval,'target')
    if(metricTarget=="maximize"){
      max <- TRUE
    }else if(metricTarget=="minimize"){
      max <- FALSE
    }else{ # Metric is not specified
      # Wrapper methods use by default RMSE for regression and Acuraccy for classification (in filter methods the metric is always specified)
      max <- ifelse(is.factor(data[,class]), TRUE, FALSE)
    }
    
    for (i in seq(maxIterations)) {
      best.value <- featureSetEval(data, class, feat.sub)
      best.feat <- NULL
      best.feat.value <- NULL
      
      # Step 1 (Exclusion): Eliminate a feature in each step, if with it, we can get a better evaluation (if not, end of the algorithm)
      for (i in seq(along = feat.sub)) {
        feat <- feat.sub[i]
        feat.prueba <- feat.sub
        feat.prueba <- feat.prueba[feat.prueba != feat]
        value <- featureSetEval(data, class, feat.prueba)
        
        # Find the feature that removing it, we can get a better evaluation
        if(max){ # Classification -> maximize
          if (is.null(best.feat.value) || best.feat.value < value) {
            best.feat.value <- value
            best.feat <- feat
          }
        }else{ # Regression -> minimize
          if (is.null(best.feat.value) || best.feat.value > value) {
            best.feat.value <- value
            best.feat <- feat
          }
        }
        
      }
      
      # If removing it we can get a better of even evaluation, we remove it
      if(max){ # Classification -> maximize
        if (is.null(best.value) || best.value <= best.feat.value) {
          best.value <- best.feat.value
          
          # Remove the selected feature
          feat.sub <- feat.sub[feat.sub != best.feat]
        } else if (stop) {
          break
        }
      }else{ # Regression -> minimize
        if (is.null(best.value) || best.value >= best.feat.value) {
          best.value <- best.feat.value
          
          # Remove the selected feature
          feat.sub <- feat.sub[feat.sub != best.feat]
        } else if (stop) {
          break
        }
      }    
      
    }
    
    # List with results
    res <- list(NULL)
    best.set.aux <- matrix(rep(0,(ncol(data)-1)), ncol=(ncol(data)-1), byrow=FALSE, dimnames=list(c(),column.names[-class.position]))
    best.set.aux[which(column.names[-class.position]%in%feat.sub)] <- 1
    res[[1]] <- best.set.aux
    res[[2]] <- best.value
    names(res) <- c("bestFeatures","bestFitness") 
    
    res
  }
    
  attr(sequentialBackwardSelectionSearch,'shortName') <- "sbs"
  attr(sequentialBackwardSelectionSearch,'name') <- "Sequential Backward Selection"
  
  return(sequentialBackwardSelectionSearch)
}

#' @author Adan M. Rodriguez
#' @author Francisco Aragón Royón
#' @title Sequential Floating Backward Selection
#' @description Generates a search function based on sequential floating backward selection. This function is called internally within the \code{\link{searchAlgorithm}} function. The sfbs method \insertCite{Pudil1994}{FSinR} starts with all the features and removes a single feature at each step with a view to improving the evaluation of the set. In addition, it checks whether adding any of the removed features, improve the value of the set.
#'
#' @return Returns a search function that is used to guide the feature selection process.
#' 
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly and performing a search process in a feature space
#' ## Classification problem
#' 
#' # Generates the filter evaluation function
#' filter_evaluator <- filterEvaluator('determinationCoefficient')
#' 
#' # Generates the search function with sfbs
#' sfbs_search <- sequentialFloatingBackwardSelection()
#' # Performs the search process directly (parameters: dataset, target variable and evaluator)
#' sfbs_search(iris, 'Species', filter_evaluator)
#' }
sequentialFloatingBackwardSelection <- function(){
  
  sequentialFloatingBackwardSelectionSearch <- function(data, class, featureSetEval) {
    if (attr(featureSetEval, 'kind') == "Individual measure") {
      stop('Only feature set measures can be used');
    }
    # Extract and eliminate the class to have only the features in the variable 'features'
    column.names <- names(data) 
    class.position <- which(column.names == class) 
    features <- column.names[-class.position] 
    feat.sub <- as.vector(features)
    excluded.features <- c()
    
    # Check for maximization-minimization
    metricTarget <- attr(featureSetEval,'target')
    if(metricTarget=="maximize"){
      max <- TRUE
    }else if(metricTarget=="minimize"){
      max <- FALSE
    }else{ # Metric is not specified
      # Wrapper methods use by default RMSE for regression and Acuraccy for classification (in filter methods the metric is always specified)
      max <- ifelse(is.factor(data[,class]), TRUE, FALSE)
    }
    
    for (i in seq(length(features) - 1)) {
      best.value <- featureSetEval(data, class, feat.sub)
      
      # Step 1 (Exclusion): Eliminate a feature in each step, if with it, we can get a better evaluation (if not, end of the algorithm)
      best.feat <- NULL
      best.feat.value <- NULL
      
      for (i in seq(along = feat.sub)) {
        feat <- feat.sub[i]
        feat.prueba <- feat.sub
        feat.prueba <- feat.prueba[feat.prueba != feat]
        value <- featureSetEval(data, class, feat.prueba)
        # Find the feature that removing it, we can get a better evaluation
        if(max){ # Classification -> maximize
          if (is.null(best.feat.value) || best.feat.value < value) {
            best.feat.value <- value
            best.feat <- feat
          }
        }else{ # Regression -> minimize
          if (is.null(best.feat.value) || best.feat.value > value) {
            best.feat.value <- value
            best.feat <- feat
          }
        }
        
      }
      
      # If removing it we can get a better of even evaluation, we remove it
      if(max){ # Classification -> maximize
        best <- best.value <= best.feat.value
      }else{ # Regression -> minimize
        best <- best.value >= best.feat.value
      }
      
      if (is.null(best.value) || best) {
        best.value <- best.feat.value
        
        # Remove the selected feature
        feat.sub <- feat.sub[feat.sub != best.feat]
        
        # Save the excluded feature for the future
        excluded.features[length(excluded.features) + 1] <- best.feat
        
        
        # Step 2: Conditional inclusion.
        crit.func.max <- featureSetEval(data, class, feat.sub) 
        continue <- TRUE
        
        # We can include 1 or more features in each step
        while (continue == TRUE) {
          worst.feat.value <- FALSE
          
          # See if including a feature of the removed, we can get a better evaluation
          for (i in seq(along = excluded.features)) {
            feat <- excluded.features[i] 
            feat.prueba <- feat.sub
            feat.prueba[length(feat.prueba) + 1] <- feat
            crit.func.eval <- featureSetEval(data, class, feat.prueba)
            
            if(max){ # Classification -> maximize
              best.critic <- crit.func.eval > crit.func.max
            }else{ # Regression -> minimize
              best.critic <- crit.func.eval < crit.func.max
            }
            
            if (best.critic) {
              worst.feat <- feat
              crit.func.max <- crit.func.eval
              worst.feat.value <- TRUE
              
              # Do not include the feature that was just removed
              if(worst.feat == best.feat) 
                worst.feat.value <- FALSE
            }
          }
          # Include the feature in the result set of features
          if (worst.feat.value == TRUE) {
            feat.sub[length(feat.sub) + 1] <- worst.feat
            excluded.features <- excluded.features[excluded.features != worst.feat]  
          } else {
            continue <- FALSE
          }
        }
      }
    }
    
    # List with results
    res <- list(NULL)
    best.set.aux <- matrix(rep(0,(ncol(data)-1)), ncol=(ncol(data)-1), byrow=FALSE, dimnames=list(c(),column.names[-class.position]))
    best.set.aux[which(column.names[-class.position]%in%feat.sub)] <- 1
    res[[1]] <- best.set.aux
    res[[2]] <- featureSetEval(data, class, feat.sub)
    names(res) <- c("bestFeatures","bestFitness") 
    
    res
  }
  attr(sequentialFloatingBackwardSelectionSearch,'shortName') <- "sfbs"
  attr(sequentialFloatingBackwardSelectionSearch,'name') <- "Sequential Floating Backward Selection"

  return(sequentialFloatingBackwardSelectionSearch)
}


