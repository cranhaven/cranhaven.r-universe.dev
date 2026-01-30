#' Distance calculation based on RandomForest Proximity or Depth
#' 
#' @param x a data.frame
#' @param y a second data.frame
#' @param rfObject \code{ranger} object
#' @param method distance calculation method, Proximity (Default) or Depth.
#' @param threads number of threads to use
#' 
#' @return a \code{dist} or a matrix object with pairwise distance of 
#' observations in x vs y (if not null)
#' 
#' @examples
#' \donttest{
#' library(ranger)
#' # proximity pairwise distances
#' rf.fit <- ranger(Species ~ ., data = iris, num.trees = 500, write.forest = TRUE)
#' distanceRandomForest(x = iris[, -5], rfObject = rf.fit, method = "Proximity", threads = 1)
#' 
#' # depth distance for train versus test subset
#' set.seed(1234L)
#' learn <- sample(1:150, 100)
#' test <- (1:150)[-learn]
#' rf.fit <- ranger(Species ~ ., data = iris[learn, ], num.trees = 500, write.forest = TRUE)
#' distanceRandomForest(x = iris[learn, -5], y = iris[test, -5], rfObject = rf.fit, method = "Depth")
#' }
#' 
#' @export
distanceRandomForest <- function(x, y = NULL, rfObject, method = "Proximity", threads = NULL) {
  method <- match.arg(method, c("Proximity", "Depth"))
  testthat::expect_is(rfObject, "ranger")
  testthat::expect_false(object = is.null(rfObject$forest), 
                         info   = "Ranger object does not contain a forest.")
  
  # set number of threads
  if (!is.null(threads)) {
    RcppParallel::setThreadOptions(numThreads = threads)
  }
  
  # Distance calculation
  if (method == "Proximity") {
    rf_dist <- proximity_distance(x = x, y = y, rfObject = rfObject)
  } else if (method == "Depth") {
    rf_dist <- depth_distance(x = x, y = y, rfObject = rfObject)
    rf_dist <- rf_dist / rfObject$num.trees
  }
  rf_dist
}



#' Get proximity matrix of an ranger object
#'
#' @param x a new dataset
#' @param y a second new dataset (Default: NULL)
#' @param rfObject \code{ranger} object
#' @param as_dist Bool, return a dist object.
#' 
#' @return a \code{dist} or a matrix object with pairwise proximity of 
#' observations in x vs y (if not null)
#'      
#' @examples
#' \donttest{
#' require(ranger)
#' rf <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' proximity_distance(x = iris[, -5], rfObject = rf)
#' 
#' set.seed(1234L)
#' learn <- sample(1:150, 100)
#' test <- (1:150)[-learn]
#' rf <- ranger(Species ~ ., data = iris[learn, ], num.trees = 500, write.forest = TRUE)
#' proximity_distance(x = iris[learn, -5], y = iris[test, -5], rfObject = rf)
#' }
#' @export
proximity_distance <- function(x, y = NULL, rfObject, as_dist=TRUE) {
  x |>
    as.matrix() |>
    terminalNodes(rfObject) -> xNodes
  if (is.null(y)) {
    d <- cpp_proximityMatrix(xNodes)
    n <- nrow(x)
    # convert to dist object
    rf_dist <- asDistObject(d, n, "RFProximity")
  } else {
    y |>
      as.matrix() |>
      terminalNodes(rfObject) -> yNodes
    rf_dist <- cpp_proximityMatrixRangerXY(xNodes, yNodes)
  }
  
  # to distance
  if (as_dist) {
    rf_dist <- sqrt(max(rf_dist) - rf_dist)
  } 
  rf_dist  
}


#' @title Depth Distance
#' 
#' @description This function returns for each observation the pairwise sum of edges  
#' between the corresponding terminal nodes over each tree in the random forest.
#' 
#' @param x A data.frame with the same columns as in the training data of the RandomForest model
#' @param y A data.frame with the same columns as in the training data of the RandomForest model
#' @param rfObject \code{ranger} object
#' 
#' @examples
#' \donttest{
#' require(ranger)
#' rf <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' depth_distance(x=iris[, -5], rfObject=rf)
#' }
#' 
#' @export
depth_distance <- function(x, y=NULL, rfObject) {
  x |>
    as.matrix() |>
    terminalNodes(rfObject) -> xNodes
  rfObject |>
    ranger_forests_to_matrix() -> rfTrees
  if (is.null(y)) {
    d <- cpp_depthMatrix(xNodes, rfTrees) 
    n <- nrow(x)
    # convert to dist object
    rf_dist <- asDistObject(d, n, "RFDepth")
  } else {
    y |>
      as.matrix() |>
      terminalNodes(rfObject) -> yNodes
    rf_dist <- cpp_depthMatrixRangerXY(xNodes, yNodes, rfTrees)
  }
  rf_dist
}


#' @title Number of Edges between Terminal Nodes
#' 
#' @description first two columns are terminal node IDs; If an ID pair do not 
#' appear in a tree -1 is inserted
#'   
#' @param rfObject \code{ranger} object
#' 
#' @return a \code{matrix} object with pairwise terminal node edge length
#'    
#' @examples
#' \donttest{
#' require(ranger)
#' rf.fit <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' edges_between_terminal_nodes(rf.fit)
#' }
#' 
#' @export
edges_between_terminal_nodes <- function(rfObject) {
  testthat::expect_is(rfObject, "ranger")
  testthat::expect_false(object = is.null(rfObject$forest), 
                         info   = "Ranger object does not contain a forest.")
  rfObject |>
    ranger_forests_to_matrix() |>
    cpp_TerminalNodeDistance()
}


#' Weighted Distance calculation
#' 
#' @param x a new dataset
#' @param y a second new dataset
#' @param weights a vector of weights
#' 
#' @return a \code{dist} or \code{matrix} object
#' 
#' @examples
#' \donttest{
#' require(ranger)
#' rf <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' terminalNodes(iris[, -5], rf)
#' }
#' 
#' @export
weightedDistance <- function(x, y=NULL, weights=NULL) {
  if (is.null(weights)) {
    weights <- seq(1, ncol(x))
  }
  if (is.null(y)) {
    d <- cpp_weightedDistance(x, weights) 
    return(asDistObject(d, nrow(x), "weightedDistance"))
  } else {
    return(cpp_weightedDistanceXY(x, y, weights))
  }
}
