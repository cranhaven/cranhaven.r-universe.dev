#' @title
#' Extend print function for cerf_gp object
#'
#' @param x A cerf_gp object.
#' @param ... Additional arguments passed to customize the results.
#'
#' @return
#' No return value. This function is called for side effects.
#'
#' @export
#'
print.cerf_gp <- function(x, ...) {
  x <- unclass(x)

  cat(" GPCERF Standard Gaussian Process exposure rate function object\n")
  cat(" function call: \n")
  cat("      ***       \n")
  print(x$fcall, ...)
  cat("      ***       \n")
  cat(" Look at summary for more details.")
}

#' @title
#' print summary of cerf_gp object
#'
#' @param object A cerf_gp object.
#' @param ... Additional arguments passed to customize the results.
#'
#' @return
#' Returns summary of data
#' @export
summary.cerf_gp <- function(object, ...) {

  cat("GPCERF standard Gaussian grocess exposure response function object\n")
  cat(paste0("\nOptimal hyper parameters(#trial: ",
             object$num_of_trial, "): \n"))
  cat(paste(" ", names(object$optimal_params), "=",
            object$optimal_params))
  cat("\n\nOptimal covariate balance: ")
  cat(paste0("\n", paste(" ", names(object$cb), "=",
                   sprintf("%.3f", object$cb))))
  cat("\n\nOriginal covariate balance: ")
  cat(paste0("\n", paste(" ", names(object$cb_org), "=",
                         sprintf("%.3f", object$cb_org))))

  cat("\n            ----***----              \n")
}



#' @title
#' Extend print function for cerf_nngp object
#'
#' @param x A cerf_nngp object.
#' @param ... Additional arguments passed to customize the results.
#'
#' @return
#' No return value. This function is called for side effects.
#'
#' @export
#'
print.cerf_nngp <- function(x, ...) {
  x <- unclass(x)

  cat(paste(" GPCERF Nearest Neighbor Gaussian Process exposure ",
            "response function object\n"))
  cat(" function call: \n")
  cat("      ***       \n")
  print(x$fcall, ...)
  cat("      ***       \n")
  cat(" Look at summary for more details.")
}


#' @title
#' print summary of cerf_nngp object
#'
#' @param object A cerf_nngp object.
#' @param ... Additional arguments passed to customize the results.
#'
#' @return
#' Returns summary of data.
#'
#' @export
#'
summary.cerf_nngp <- function(object, ...) {

  cat(paste0("GPCERF nearest neighbore Gaussian process exposure response",
             " function object summary\n"))

  cat(paste0("\nOptimal hyper parameters(#trial: ",
             object$num_of_trial, "): \n"))
  cat(paste(" ", names(object$optimal_params), "=",
            object$optimal_params))
  cat("\n\nOptimal covariate balance: ")
  cat(paste0("\n", paste(" ", names(object$cb), "=",
                         sprintf("%.3f", object$cb))))
  cat("\n\nOriginal covariate balance: ")
  cat(paste0("\n", paste(" ", names(object$cb_org), "=",
                         sprintf("%.3f", object$cb_org))))

  cat("\n            ----***----              \n")
}
