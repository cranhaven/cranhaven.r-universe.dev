RFSM_diffS <- function(data, S, e1, e2, penalization = FALSE) {
  result <- NULL
  for (feature in S) {
    if (is.null(feature))  {
      result <- RFSM_diff(data, feature, e1, e2)
    } else {
      f_column <- unlist(as.data.frame(data[, feature, drop = FALSE]))
      if (is.discrete(f_column)) {
        result <- (if (is.null(result)) 0 else result) || RFSM_diff(data, feature, e1, e2)
      } else {
        if (penalization) {
          result <- min(result, RFSM_diff(data, feature, e1, e2))
        } else {
          result <- max(result, RFSM_diff(data, feature, e1, e2))
        }
      }
    }
  }
  return(as.numeric(result))
}

RFSM_diff <- function(data, f, e1, e2) {
 f_column <- unlist(as.data.frame(data[, f, drop = FALSE]))
 if (is.discrete(f_column)) {
   if (e1[f] == e2[f]) {
     return(0)
   } else
     return(1)
 } else {
   f_min <- min(f_column)
   f_max <- max(f_column)
   return(as.numeric( (abs(e1[f] - e2[f]) / (f_max - f_min))))
 }
}

#' @importFrom rlang .data
selectKNeighbours <- function(data, S, examples, example, k, penalization = FALSE) {
  result <- examples
  result <- result %>%
    rowwise() %>%
    do(row = as_tibble(.data)) %>%
    mutate(rfsm_distance = RFSM_diffS(data, S, example, row, penalization)) %>%
    unnest(cols = c(row)) %>%
    arrange(desc(.data$rfsm_distance)) %>%
    top_n(k, .data$rfsm_distance)
  return(result)
}

#' @author Alfonso Jiménez-Vílchez
#' @title Relief Feature Set Measure evaluation measure
#' @description Generates an evaluation function that applies Feature set measure based on Relief (set measure). Described in \insertCite{Arauzo2004}{FSinR}. This function is called internally within the \code{\link{filterEvaluator}} function.
#' 
#' @param iterations Number of iterations
#' @param kNeightbours Number of neighbours
#'
#' @return Returns a function that is used to generate an evaluation set measure (between -1 and 1) using RFSM value for the selected features.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#' @import tidyr
#' @import prodlim
#' @import dplyr
#'
#' @examples
#'\dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly to evaluate a set of features
#' ## Classification problem
#' 
#' # Generate the evaluation function with Cramer
#' RFSM_evaluator <- ReliefFeatureSetMeasure()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' RFSM_evaluator(iris,'Species',c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'))
#' }
ReliefFeatureSetMeasure <- function(iterations = 5, kNeightbours = 4) {

  RFSMevaluator <- function(original_data, class, features) {

    if (!length(features)) {
      return(0);
    }

    # Group all proyected examples in lists by their class
    # Get values of the features
    features <- unlist(features)
    feature.data <- original_data[, features, drop = FALSE]
    feature.classes <- as.data.frame(original_data[, class, drop = FALSE])
    feature.list <- unique(feature.classes)

    hash.table <- list()
    for (i in 1:nrow(feature.list)) {
      hash.table[[i]] <- subset(feature.data, FALSE) # Get dataframe with columns names but no rows.
    }
    for (i in 1:nrow(feature.data)) {
      class_index <- which(feature.list == as.character(feature.classes[i, ]))
      hash.table[[class_index]][nrow(hash.table[[class_index]]) + 1, ] <- feature.data[i, ] # rbind does not work correctly
    }
    feature.prob <- list()
    for (i in 1:nrow(feature.list)) {
      feature.prob[[i]] <- nrow(hash.table[[i]]) / nrow(original_data)
    }

    w <- 0;

    for (i in 1:iterations) {
      e1.row <- sample(nrow(original_data), 1)
      e1 <- original_data[e1.row, ]
      e1.data <- e1[, features, drop = FALSE]
      for (j in 1:nrow(feature.list)) {
        examples <- hash.table[[j]][, features, drop = FALSE]
        if (e1[, class] == feature.list[j, ]) {
          position <- row.match(e1.data, examples)
          if (!is.na(position)) {
            examples <- examples[-c(position),, drop = FALSE]
          }
          if (nrow(examples) > 0) {
            w <- w - sum(selectKNeighbours(original_data, features, examples, e1.data, kNeightbours)$rfsm_distance) / kNeightbours
          }
        } else {
          w <- w + feature.prob[[j]] * sum(selectKNeighbours(original_data, features, examples, e1.data, kNeightbours, TRUE)$rfsm_distance) / kNeightbours
        }
      }
    }

    return(w / iterations)
  }
  attr(RFSMevaluator,'shortName') <- "RFSM"
  attr(RFSMevaluator,'name') <- "ReliefFeatureSetMeasure"
  attr(RFSMevaluator,'target') <- "maximize"
  attr(RFSMevaluator,'kind') <- "Set measure"
  attr(RFSMevaluator,'needsDataToBeDiscrete') <- FALSE
  attr(RFSMevaluator,'needsDataToBeContinuous') <- FALSE

  return(RFSMevaluator)
}


#' @author Alfonso Jiménez-Vílchez
#' @title Relief Feature Set Measure evaluation measure
#' @description Generates an evaluation function that applies Feature set measure based on Relief (set measure). Described in \insertCite{Arauzo2004}{FSinR}. This function is called internally within the \code{\link{filterEvaluator}} function.
#' 
#' @param iterations Number of iterations
#' @param kNeightbours Number of neighbours
#'
#' @return Returns a function that is used to generate an evaluation set measure (between -1 and 1) using RFSM value for the selected features.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#' @import tidyr
#' @import prodlim
#' @import dplyr
#'
#' @examples
#'\dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly to evaluate a set of features
#' ## Classification problem
#' 
#' # Generate the evaluation function with Cramer
#' RFSM_evaluator <- ReliefFeatureSetMeasure()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' RFSM_evaluator(iris,'Species',c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'))
#' }
normalizedReliefFeatureSetMeasure <- function(iterations = 5, kNeightbours = 4) {
  originalRelief <- ReliefFeatureSetMeasure(iterations = 5, kNeightbours = 4)
  normalizedRFSMEvaluator <- function(data, class, features) {
    originalValue <- originalRelief(data, class, features)
    return(originalValue / 2 + 0.5)
  }
  
  attr(normalizedRFSMEvaluator,'shortName') <- "normalizedRFSM"
  attr(normalizedRFSMEvaluator,'name') <- "Normalized ReliefFeatureSetMeasure"
  attr(normalizedRFSMEvaluator,'target') <- "maximize"
  attr(normalizedRFSMEvaluator,'kind') <- "Set measure"
  attr(normalizedRFSMEvaluator,'needsDataToBeDiscrete') <- FALSE
  attr(normalizedRFSMEvaluator,'needsDataToBeContinuous') <- FALSE
  
  return(normalizedRFSMEvaluator)
}

