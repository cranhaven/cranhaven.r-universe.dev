#' Print w_naive_bayes
#'
#' Prints a summary of w_naive_bayes model
#'
#' @param x w_naive_bayes object
#' @param ... additional arguments.
#'
#' @return Summary of "w_naive_bayes" object.
#'
#' @rdname print.w_naive_bayes
#' @keywords internal
#' @export

print.w_naive_bayes <- function(x, ...){
  if (x$discretize) {
    print.w_discrete_naive_bayes(x, ...)
  }else {
    print.w_gaussian_naive_bayes(x, ...)
  }
  return(invisible(x))
}

#' @rdname print.w_naive_bayes
#' @keywords internal
#' @export
print.w_gaussian_naive_bayes <- function(x, ...){
  cat("Naive Bayes model with Gaussian densities",
      "\n ",
      "k = ", x$k_classes,
      "\n ",
      "class names = ", paste(as.character(x$class_names), collapse = ", "),
      "\n ",
      "priors = ", "{", paste(x$class_names, "=", x$priors, collapse = ", ", sep = ""), "}",
      "\n ",
      "n classes = ", "{", paste("n_", x$class_names, "=", x$n_classes, collapse = ", ", sep = ""), "}",
      "\n ",
      "marginal distributions : Gaussian densities", sep = "")

  return(invisible(x))
}


#' @rdname print.w_naive_bayes
#' @keywords internal
#' @export
print.w_discrete_naive_bayes <- function(x, ...){
  cat("Naive Bayes model with discretized variables",
      "\n ",
      "k = ", x$k_classes,
      "\n ",
      "class names = ", paste(as.character(x$class_names), collapse = ", "),
      "\n ",
      "priors = ", "{", paste(x$class_names, "=", x$priors, collapse = ", ", sep = ""), "}",
      "\n ",
      "n classes = ", "{", paste("n_", x$class_names, "=", x$n_classes, collapse = ", ", sep = ""), "}",
      "\n ",
      "marginal distributions : binomial probabilities", sep = "")

  return(invisible(x))
}
