# Exhaustive searchs
#' @author Adan M. Rodriguez
#' @author Francisco Aragón Royón
#' @title Breadth First Search (exhaustive search)
#' @description Generates a search function based on the breadth first search. This function is called internally within the \code{\link{searchAlgorithm}} function. Breadth First Search searches the whole features subset in breadth first order \insertCite{Kozen1992}{FSinR}.
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
#' # Generates the search function with Breadth first
#' bfs_search <- breadthFirst()
#' # Performs the search process directly (parameters: dataset, target variable and evaluator)
#' bfs_search(iris, 'Species', filter_evaluator)
#' }
breadthFirst <- function() {
  
  breadthFirstSearch <- function(data, class, featureSetEval) {
    
    if (attr(featureSetEval, 'kind') == "Individual measure") {
      stop('Only feature set measures can be used');
    }
    # Extract and eliminate the class to have only the features in the variable 'features'
    column.names <- names(data)
    class.position <- which(column.names == class)
    features <- column.names[-class.position]
    
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
    
    # In best.set, we store the features that are part of the solution
    best.set <- NULL
    best.value <- NULL
    
    # Queue the root of the tree
    queue <- NULL
    queue[[length(queue)+1]] <- list(list(), as.list(features))
    
    # Visite each feature
    while (length(queue) > 0) {
      # Pop (introduce) an unvisited node 
      node <- queue[[1]] 
      trunk <- node[[1]] 
      branches <- node[[2]]
      queue[[1]] <- NULL
      
      # visit each branch of the current node
      for (i in seq(along=branches)) {
        set <- c(trunk, branches[[i]])
        
        # Evaluate and check if better
        value <- featureSetEval(data, class, unlist(set))
        
        # Store the new feature if is better
        if(max){ # Classification -> Maximize
          if (is.null(best.value) || value > best.value) {
            best.value <- value
            best.set <- set
          }
        }else{ # Regression -> Minimize
          if (is.null(best.value) || value < best.value) {
            best.value <- value
            best.set <- set
          }
        }
        
        # Generate branch nodes if there are remaining features to combine. In breadth
        n <- length(branches)
        if (i < n) {
          queue[[length(queue)+1]] <- list(set, branches[(i+1):n])
        }
      }
      
    }
    
    # List with results
    res <- list(NULL)
    best.set.aux <- matrix(rep(0,length(features)), ncol=length(features), byrow=FALSE, dimnames=list(c(),features))
    pos <- match(unlist(best.set),features)
    best.set.aux[pos] <- 1
    
    res[[1]] <- best.set.aux
    res[[2]] <- best.value
    names(res) <- c("bestFeatures","bestFitness") 
    
    res
  }
  attr(breadthFirstSearch,'shortName') <- "breadthFirstSearch"
  attr(breadthFirstSearch,'name') <- "Breadth First Search"
  
  return(breadthFirstSearch)
}

#' @author Francisco Aragón Royón
#' @title Deep First Search (exhaustive search)
#' @description Generates a search function based on the deep first search. This function is called internally within the \code{\link{searchAlgorithm}} function. Deep First Search searches the whole features subset in deep first order \insertCite{Kozen1992}{FSinR}.
#'
#' @return Returns a search function that is used to guide the feature selection process.
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
#' # Generates the search function with Deep first
#' dfs_search <- deepFirst()
#' # Performs the search process directly (parameters: dataset, target variable and evaluator)
#' dfs_search(iris, 'Species', filter_evaluator)
#' }
deepFirst <- function() {
  
  deepFirstSearch <- function(data, class, featureSetEval) {
    
    if (attr(featureSetEval, 'kind') == "Individual measure") {
      stop('Only feature set measures can be used');
    }
    # Extract and eliminate the class to have only the features in the variable 'features'
    column.names <- names(data)
    class.position <- which(column.names == class)
    features <- column.names[-class.position]
    
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
    
    # In best.set, we store the features that are part of the solution
    best.set <- NULL
    best.value <- NULL
    
    # Queue the root of the tree
    queue <- NULL
    queue[[length(queue)+1]] <- list(list(), as.list(features))
    
    # Visite each feature
    while (length(queue) > 0) { 
      # Pop (introduce) an unvisited node 
      node <- queue[[1]] 
      trunk <- node[[1]] 
      branches <- node[[2]]
      queue[[1]] <- NULL
      
      # visit the first branch of the current node
      set <- c(trunk, branches[[1]])
      # Evaluate and check if better
      value <- featureSetEval(data, class, unlist(set))
      
      # Store the new feature if is better
      if(max){ # Classification -> Maximize
        if (is.null(best.value) || value > best.value) {
          best.value <- value
          best.set <- set
        }
      }else{ # Regression -> Minimize
        if (is.null(best.value) || value < best.value) {
          best.value <- value
          best.set <- set
        }
      }
      
      
      # Generate branch nodes if there are remaining features to combine. In deep
      n <- length(branches)
      if(n>1){
        queue <- append(queue, list(list(trunk, branches[2:n])), 0)
        queue <- append(queue, list(list(set, branches[2:n])), 0)      
      }
    }
    
    # List with results
    res <- list(NULL)
    best.set.aux <- matrix(rep(0,length(features)), ncol=length(features), byrow=FALSE, dimnames=list(c(),features))
    pos <- match(unlist(best.set),features)
    best.set.aux[pos] <- 1
    
    res[[1]] <- best.set.aux
    res[[2]] <- best.value
    names(res) <- c("bestFeatures","bestFitness") 
    
    res
  }
  attr(deepFirstSearch,'shortName') <- "deepFirstSearch"
  attr(deepFirstSearch,'name') <- "Deep First Search"
  
  return(deepFirstSearch)
}
