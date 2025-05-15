#' get.default.parameters
#'
#' @keywords internal
#'
get.default.parameters <- function(mcall, myFormals) {
  ## formals with default arguments
  for ( v in names(myFormals)){
    if (!(v %in% names(mcall)))
      mcall[v] <- myFormals[v]  ## if arg is missing I add it
  }
  return(mcall)
}

#' select_on_class
#'
#' @keywords internal
#'
select_on_class <- function(.data, clases = "numeric") {
  .data[, sapply(.data, function(vec, clss) any(class(vec) %in% clss), clss = clases), drop  = FALSE]
}

#' contr.dummy
#'
#' @description Returns a matrix of contrasts for the \code{\link[kknn]{train.kknn}}.
#'
#' @param n A vector containing levels of a factor, or the number of levels.
#' @param contrasts A logical value indicating whether contrasts should be computed.
#'
#' @return A matrix with n rows and n-1 columns for contr.ordinal, a matrix with n rows and n columns for contr.dummy and a vector of length n for contr.metric.
#'
#' @export contr.dummy
#'
contr.dummy <- function (n, contrasts = TRUE) {
  if (length(n) <= 1) {
    if (is.numeric(n) && length(n) == 1 && n > 1)
      levels <- 1:n
    else stop("contrasts are not defined for 0 degrees of freedom")
  }
  else levels <- n
  lenglev <- length(levels)
  cont <- array(0, c(lenglev, lenglev), list(levels, levels))
  cont[col(cont) == row(cont)] <- 1
  cont
}

#' contr.ordinal
#'
#' @description Returns a matrix of contrasts for the \code{\link[kknn]{train.kknn}}.
#'
#' @param n A vector containing levels of a factor, or the number of levels.
#' @param contrasts A logical value indicating whether contrasts should be computed.
#'
#' @return A matrix with n rows and n-1 columns for contr.ordinal, a matrix with n rows and n columns for contr.dummy and a vector of length n for contr.metric.
#'
#' @export contr.ordinal
#'
contr.ordinal <- function (n, contrasts = TRUE) {
  if (length(n) <= 1) {
    if (is.numeric(n) && length(n) == 1 && n > 1)
      levels <- 1:n
    else stop("contrasts are not defined for 0 degrees of freedom")
  }
  else levels <- n
  lenglev <- length(levels)
  cont <- array(0.5, c(lenglev, lenglev - 1), list(levels,
                                                   NULL))
  cont[lower.tri(cont)] <- -0.5
  cont
}

#' contr.metric
#'
#' @description Returns a matrix of contrasts for the \code{\link[kknn]{train.kknn}}.
#'
#' @param n A vector containing levels of a factor, or the number of levels.
#' @param contrasts A logical value indicating whether contrasts should be computed.
#'
#' @return A matrix with n rows and n-1 columns for contr.ordinal, a matrix with n rows and n columns for contr.dummy and a vector of length n for contr.metric.
#'
#' @export contr.metric
#'
contr.metric <- function (n, contrasts = TRUE) {
  if (length(n) <= 1) {
    if (is.numeric(n) && length(n) == 1 && n > 1)
      levels <- 1:n
    else stop("contrasts are not defined for 0 degrees of freedom")
  }
  else levels <- n
  lenglev <- length(levels)
  cont <- array((1:lenglev) - (1 + lenglev)/2, c(lenglev, 1),
                list(levels, NULL))
  cont
}

#' scaler
#'
#' @description Returns a scaled data.frame.
#'
#' @param df A data.frame only with numeric variables.
#'
#' @return A data.frame.
#'
#' @export scaler
#'
scaler <- function (df) {
  m <- apply(df, 2, mean)
  s <- apply(df, 2, sd)

  return(list(
    "apl"   = function(x) data.frame(t((t(x) - m) / s)),
    "inv"   = function(x) data.frame(t((t(x) * s) + m)),
    "invar" = function(x, v) x * s[v] + m[v]
  ))
}

#' max_col
#'
#' @keywords internal
#'
max_col <- function(m){
  base::max.col(apply(m, 1, function(x) max(x, na.rm = TRUE)) == m)
}

#' numeric_to_predict
#'
#' @keywords internal
#'
numeric_to_predict <- function(real, predic.var = NULL, niveles = NULL) {
  if(is.numeric(predic.var)) {
    if(is.null(niveles)){
      numCategories <-  length(levels(real))
      #We must specify the possible values that the factor type object can take
      #Then we specify the labels that must have the same size as the levels
      predic.var <- factor(predic.var, levels = 1:numCategories,labels = levels(real))
    }
    else{
      numCategories <-  length(niveles)
      predic.var <- factor(predic.var, levels = 1:numCategories,labels = niveles)
    }
  }
  predic.var
}

#' type_correction
#'
#' @keywords internal
#'
type_correction <- function(model, prediction, fix){
  var_type <- model$prmdt$type
  .levels  <- model$prmdt$levels
  if(var_type != "numeric" && var_type != "integer" && fix){
    names(prediction) <- NULL
    return(factor(prediction, levels = .levels))
  }else{
    return(prediction)
  }
}

#' original_model
#'
#' @keywords internal
#'
original_model <- function(x){
  class(x) <- class(x)[!str_detect(class(x), "prmdt")]
  x$prmdt <- NULL
  return(x)
}

#' get_test_less_predict
#'
#' @keywords internal
#'
get_test_less_predict <- function(data, var.pred){
  if(var.pred %in% colnames(data)){
    return(data[,-which(colnames(data) == var.pred)])
  }
  return(data)
}

#' Printing prmdt models
#'
#' @param x A prmdt models
#' @param ... optional arguments to print o format method
#'
#' @return a print information of a model.
#'
#' @export print.prmdt
#' @export
#'
print.prmdt <- function(x, ...){
  print(original_model(x), ...)
}

#' Printing prmdt prediction object
#'
#' @param x A prmdt prediction object
#' @param ... optional arguments to print o format method
#'
#' @return a print prediction of a model.
#'
#' @export print.prediction.prmdt
#' @export
#'
print.prediction.prmdt <- function(x, ...){
  print(x$prediction, ...)
}

#' Printing prmdt index object
#'
#' @param x A prmdt index object
#' @param ... optional arguments to print o format method
#'
#' @importFrom utils capture.output
#'
#' @return a print of the results of a prediction model.
#'
#' @export print.indexes.prmdt
#' @export
#'
print.indexes.prmdt <- function(x, ...) {
  if(is.null(x$confusion.matrix)) {
    out <- paste(capture.output(unlist(x)), collapse = "\n")
  } else {
    out <- c("\nConfusion Matrix:",capture.output(x$confusion.matrix))
    out <- paste(out, collapse = "\n")
    out <- paste0(out, sprintf("\n\nOverall Accuracy: %3.4f\nOverall Error: %9.4f\n", x$overall.accuracy, x$overall.error))
    out.aux <- do.call(sprintf, c(paste0(rep("%13s", length(x$category.accuracy)), collapse = ""), as.list(names(x$category.accuracy))))
    out.aux <- paste0("\nCategory Accuracy:\n\n",out.aux, "\n",
                      do.call(sprintf, c(paste0(rep("%13f", length(x$category.accuracy)), collapse = ""), as.list(as.numeric(x$category.accuracy)))))
    out <- paste0(out, out.aux)
  }
  cat(out)
}

#' Plotting prmdt models
#'
#' @param x A prmdt models
#' @param ... optional arguments to print o format method
#'
#' @return a plot of a model.
#'
#' @export plot.prmdt
#' @export
#'
plot.prmdt <- function(x, ...){
  x <- original_model(x)
  plot(x, ...)
}

#' Plotting prmdt ada models
#'
#' @param x A ada prmdt model
#' @param ... optional arguments to print o format method
#'
#' @return a plot of the importance of variables.
#'
#' @export
#'
varplot <- function(x, ...){
  x <- original_model(x)
  ada::varplot(x, ...)
}

#' dummy.data.frame
#'
#' @keywords internal
#'
dummy.data.frame <- function (data) {
  if(is.null(data)) {
    return(NULL)
  }

  res <- data.frame(row.names = row.names(data))
  for (x in names(data)) {
    if(is.numeric(data[[x]])) {
      res[ , x] <- data[[x]]
    } else {
      for (categoria in unique(data[[x]])) {
        res[[paste0(x, '.', categoria)]] <- as.numeric(data[[x]] == categoria)
      }
    }
  }
  return(res)
}
