#' Print a Summary of a \code{mmc} Object
#'
#' This is a method for the function \code{print()} for objects of the
#' class \code{mmc}.
#'
#' @return The \code{mmc} object is returned invisibly.
#'
#' @inheritParams base::print
#' @export
#'
#' @example /inst/examples/print_mmc_example.R
#'
print.mmc <- function(x, digits = getOption("digits"), ...){

  cat("\nMaximized Monte Carlo\n")
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")

  out <- character()
  out <- c(out, paste(names(x$S0), "=", format(signif(x$S0,max(1L, digits - 2L)))))
  out <- c(out, paste("N", "=", format(signif(x$N,max(1L, digits - 2L)))))
  cat(strwrap(paste(out, collapse = ", ")), "\n", sep = "")
  if (!is.null(x$lmc)){
      cat(paste("Local Monte Carlo: p-value", "=",
                format(signif(x$lmc$pval, max(1L, digits - 2L)))),sep = "\n")
  }

  if (is.null(x$rejection)){
      out <- character()
      out <- c(out, paste("Maximized Monte Carlo: p-value", "=",
                          format(signif(x$pval,max(1L, digits - 2L)))))
      cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
  } else if (x$pval == 1 || !all(x$rejection)) {
      out <- character()
      out <- c(out, paste("Maximized Monte Carlo: p-value", "=",
                          format(signif(x$pval,max(1L, digits - 2L)))))
      cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
      cat("\nRejection:\n")
      print(x$rejection)
  } else {
      out <- character()
      out <- c(out, paste("Maximized Monte Carlo: p-value", ">=",
                          format(signif(x$pval,max(1L, digits - 2L)))))
      cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
      cat("\nRejection:\n")
      print(x$rejection)
  }
  invisible(x)

}

#' Print a Summary of a \code{mc} Object
#'
#' This is a method for the function \code{print()} for objects of the
#' class \code{mc}.
#'
#' @return The \code{mc} object is returned invisibly.
#'
#' @inheritParams base::print
#' @export
#'
#' @example /inst/examples/print_mc_example.R
#'
print.mc <- function(x, digits = getOption("digits"), ...){

  cat("\nMonte Carlo with Tie-Breaker\n")
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  out <- character()
  out <- c(out, paste(names(x$S0), "=", format(signif(x$S0,max(1L, digits - 2L)))))
  out <- c(out, paste("N", "=", format(signif(x$N,max(1L, digits - 2L)))))
  out <- c(out, paste("p-value", "=", format(signif(x$pval,max(1L, digits - 2L)))))
  cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
  invisible(x)

}
