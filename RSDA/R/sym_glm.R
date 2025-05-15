#' Lasso, Ridge and and Elastic Net Linear regression model to interval variables
#' @name sym.glm
#' @aliases sym.glm
#' @author Oldemar Rodriguez Rojas
#' @description Execute Lasso, Ridge and and Elastic Net Linear regression model to interval variables.
#' @usage sym.glm(sym.data, response = 1, method = c('cm', 'crm'),
#' alpha = 1, nfolds = 10, grouped = TRUE)
#' @param sym.data Should be a symbolic data table read with the function read.sym.table(...).
#' @param response The number of the column where is the response variable in the interval data table.
#' @param method 'cm' to generalized Center Method and 'crm' to generalized Center and Range Method.
#' @param alpha alpha=1 is the lasso penalty, and alpha=0 the ridge penalty. 0<alpha<1 is the elastic net method.
#' @param nfolds Number of folds - default is 10. Although nfolds can be as large as the sample size
#' (leave-one-out CV), it is not recommended for large datasets. Smallest value allowable
#' is nfolds=3
#' @param grouped This is an experimental argument, with default TRUE, and can be ignored by most users.
#'
#' @return An object of class 'cv.glmnet' is returned, which is a list with the ingredients of the cross-validation fit.
#' @references Rodriguez O. (2013). A generalization of Centre and Range method for fitting a linear
#' regression model to symbolic interval data using Ridge Regression, Lasso
#' and Elastic Net methods. The IFCS2013 conference of the International Federation of
#' Classification Societies, Tilburg University Holland.
#' @seealso sym.lm
#'
#' @keywords Symbolic Regression Lasso Ridge
#' @export
#' @importFrom  glmnet cv.glmnet
#'
sym.glm <- function(sym.data, response = 1, method = c("cm", "crm"), alpha = 1, nfolds = 10,
                    grouped = TRUE) {
  all_interval <- all(sapply(sym.data, function(x) any(class(x) %in% "symbolic_interval")))
  if (!all_interval) {
    stop("All variables have to be of the same type")
  }
  method <- match.arg(method)
  if (method == "cm") {
    centers <- interval.centers(sym.data)
    centers <- as.matrix(centers)
    model <- glmnet::cv.glmnet(
      x = centers[, -response],
      y = centers[, response],
      nfolds = nfolds,
      grouped = grouped,
      alpha = alpha
    )
    class(model) <- c("symbolic_glm_cm", class(model))
    return(model)
  }
  if (method == "crm") {
    ## Center Model
    centers <- interval.centers(sym.data)
    centers <- as.matrix(centers)
    model_centers <- glmnet::cv.glmnet(
      x = centers[, -response],
      y = centers[, response],
      nfolds = nfolds,
      grouped = grouped,
      alpha = alpha
    )
    # Range Model
    range <- interval.ranges(sym.data)
    range <- as.matrix(range)
    model_range <- glmnet::cv.glmnet(
      x = range[, -response],
      y = range[, response],
      nfolds = nfolds,
      grouped = grouped,
      alpha = alpha
    )
    model <- list(CenterModel = model_centers, RangeModel = model_range)
    class(model) <- c("symbolic_glm_crm")
    return(model)
  }
}
