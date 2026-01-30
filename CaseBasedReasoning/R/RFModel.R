#' RandomForest Model for Searching Similar Cases
#' 
#' This class uses the proximity or depth matrix of the RandomForest algorithm 
#' as a similarity matrix of training and query 
#' observations. By default all cases with at least one missing values are dropped 
#' from learning, calculating the distance matrix and searching for similar
#' cases. 
#' 
#' @references 
#' Englund and Verikas. A novel approach to estimate proximity in a random 
#' forest: An exploratory study.
#' @export
RFModel <- R6Class(
  classname = "RFModel",
  inherit = CBRBase,
  public=list(
    #' @field model the statistical model
    model        = 'ranger',
    #' @field model_params model arguments
    model_params = list(write.forest = T, verbose = T),
    #' @field  dist_method Distance method  
    dist_method  = "Depth",
    #' @description 
    #' Prints information of the initialized object
    print = function() {
      message("Case-Based-Reasoning with RandomForests")
      message("---------------------------------------")
      message("Model     : ", paste(self$model, collapse = ", "))
      message("Endpoints : ", paste(self$endPoint, collapse = ", "))
      message("Variables : ", paste(self$terms, collapse = ", "))
      message("Method    : ", paste(self$dist_method, collapse = ", "))
      message("Trained   : ", ifelse(is.null(self$model_fit), FALSE, TRUE))
    },
    #' @description 
    #' Initialize a RandomForest object for searching similar cases.
    #'
    #' @param formula Object of class formula or character describing the model fit.
    #' @param data Training data of class data.frame
    #' @param ... ranger RandomForest arguments 
    initialize = function(formula, data, ...) {
      super$initialize(formula, data)
      self$model_params <- list(...)
      self$model_params$write.forest <- T
      self$model_params$verbose <- T
    },
    #' @description 
    #' Fit the RandomForest
    #' 
    #' @param x Training data of class data.frame
    fit = function() {
      self$data |>
        dplyr::select(c(self$endPoint, self$terms)) -> train_tbl
      train_tbl <- private$check_data(train_tbl)
      
      # Parameters
      # train regression model
      func <- get(self$model, envir = as.environment('package:ranger'))
      params <- self$model_params
      params$data <- train_tbl
      params$formula <- self$formula
      self$model_fit <- pryr::do_call(func, params)
    },
    #' @description 
    #' Set the distance method. Available are Proximity and Depth
    #' 
    #' @param method Distance calculation method (default: Proximity)
    set_distance_method=function(method = "Depth") {
      method <- match.arg(method, c("Proximity", "Depth"))
      self$dist_method <- method
    }
  ),
  private = list(
    get_distance_matrix = function(x, query = NULL) {
      testthat::expect_is(self$model_fit, "ranger")
      self$distMat <- distanceRandomForest(x        = private$to_int(x),
                                           y        = private$to_int(query), 
                                           method   = self$dist_method,
                                           rfObject = self$model_fit)
    }
  )
)