#' Print booster
#'
#' Prints a summary of booster model
#'
#' @param x booster object
#' @param ... additional arguments.
#'
#' @return Summary of "booster" object.
#' @importFrom utils tail
#'
#' @rdname print.booster
#' @keywords internal
#' @export

print.booster <- function(x, ...){
  if (class(x) != "booster") {
    stop("object class must be 'booster'")
  }

  pp <- get(paste0("print.", x$method, "_adaboost"))
  pp(x)
  return(invisible(x))
}

#' @rdname print.booster
#' @keywords internal
#' @export
print.discrete_adaboost <- function(x, ...){
  if (class(x) != "booster") {
    stop("object class must be 'booster'")
  }

  max_iter <- x$max_iter
  lambda <- x$lambda
  err_train <- x$err_train
  err_test <- x$err_test
  k_classes <- x$k_classes
  weighted_bootstrap <- x$weighted_bootstrap
  bag_frac <- x$bag_frac

  cat(ifelse(k_classes == 2, "Binary classification", "Multinomial classification"),
      "using Discrete Adaboost\n",
      "k =", k_classes,
      "\n",
      "bootstrap =", weighted_bootstrap,
      "\n",
      ifelse(weighted_bootstrap, "", paste("bag_frac =", bag_frac, "\n")),
      "Iteration =", length(x$models),
      "\n",
      "lambda =", lambda,
      "\n",
      "err_train =", formatC(tail(err_train, 1), digits = 3, format = "f"),
      "\n",
      ifelse(is.null(err_test), "", paste("err_test",
                                          formatC(tail(err_test, 1), digits = 3,
                                                  format = "f"), "\n")))

  return(invisible(x))
}


#' @rdname print.booster
#' @keywords internal
#' @export
print.real_adaboost <- function(x, ...){
  if (class(x) != "booster") {
    stop("object class must be 'booster'")
  }

  max_iter <- x$max_iter
  lambda <- x$lambda
  err_train <- x$err_train
  err_test <- x$err_test
  k_classes <- x$k_classes
  weighted_bootstrap <- x$weighted_bootstrap
  bag_frac <- x$bag_frac

  cat(ifelse(k_classes == 2, "Binary classification", "Multinomial classification"),
      "using Real Adaboost\n",
      "k =", k_classes,
      "\n",
      "bootstrap =", weighted_bootstrap,
      "\n",
      ifelse(weighted_bootstrap, "", paste("bag_frac =", bag_frac, "\n")),
      "Iteration =", length(x$models),
      "\n",
      "lambda =", lambda,
      "\n",
      "err_train =", formatC(tail(err_train, 1), digits = 3, format = "f"),
      "\n",
      ifelse(is.null(err_test), "", paste("err_test",
                                          formatC(tail(err_test, 1), digits = 3,
                                                  format = "f"), "\n")))

  return(invisible(x))
}
