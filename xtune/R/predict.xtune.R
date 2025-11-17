#' Model predictions based on fitted \code{xtune} object
#'
#' \code{predict_xtune} produces predicted values fitting an xtune model to a new dataset
#' @param object Fitted 'xtune' model object.
#' @param newX Matrix of values at which predictions are to be made.
#' @param type Type of prediction required. For "linear" models it gives the fitted values. Type "response" gives the fitted probability scores of each category for "binary" or "multiclass" outcome.  Type "class" applies to "binary" or "multiclass" models, and produces the class label corresponding to the maximum probability.
#' @param ... Not used
#' @details \code{coef} and \code{predict} methods are provided as a convenience to extract coefficients and make prediction. \code{predict_xtune} simply calculate the predicted value using the estimated coefficients returned by \code{xtune}.
#' @return A vector of predictions
#' @seealso \code{xtune}, \code{coef_xtune}
#' @examples
#'
#' ## If no Z provided, perform Empirical Bayes tuning
#' ## simulate linear data
#' set.seed(9)
#' data(example)
#' X <- example$X
#' Y <- example$Y
#' Z <- example$Z
#'
#' \donttest{
#' fit.eb <- xtune(X,Y)
#' coef_xtune(fit.eb)
#' predict_xtune(fit.eb,X)
#' }
#'
#'
#' ## Feature specific shrinkage based on external information Z:
#'
#' ## simulate multi-categorical data
#' data(example.multiclass)
#' X <- example.multiclass$X
#' Y <- example.multiclass$Y
#' Z <- example.multiclass$Z
#' \donttest{
#' fit <- xtune(X,Y,Z,family = "multiclass")
#'
#'
#' ## Coef and predict methods
#' coef_xtune(fit)
#' predict_xtune(fit,X, type = "class")
#' }

#' @export

predict_xtune <- function(object, newX, type = c("response","class"), ...) {
        type = match.arg(type)
        ## check new X input
        if (missing(newX)){
            stop("You need to supply a value for 'newX'")
            } else if (!(typeof(newX) %in% c("double", "integer"))) {
                    stop("New X contains non-numeric values")
                    } else if (!is.matrix(newX)) {
                            stop("New X is not a matrix")
                            } else if (object$family == "linear") {
                                if(length(object$beta.est[-1]) != ncol(newX)){
                                    stop("New X does not have the same number of columns as X train")
                                }
                            } else if (object$family != "linear"){
                                if(length(object$beta.est[[1]][-1]) != ncol(newX)){
                                    stop("New X does not have the same number of columns as X train")
                                }
                                    }

        # Check the family of Y
        if (type == "class" & object$family == "binary"){
                predicted <- predict(object$model, newx = newX, s = object$lambda, type = "class")
        } else if(type == "response" & object$family == "binary"){
                predicted <- as.data.frame(predict(object$model, newx = newX, s = object$lambda, type = "response"))
        } else if(type == "class" & object$family == "multiclass"){
                predicted <- predict(object$model, newx = newX, s = object$lambda, type = "class")
        } else if(type == "response" & object$family == "multiclass"){
                predicted <- as.data.frame(predict(object$model, newx = newX, s = object$lambda, type = "response"))
        } else {
                predicted <- object$beta.est[1] + newX %*% object$beta.est[-1]
        }

    return(drop(predicted))
}
