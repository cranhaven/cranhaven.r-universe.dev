#' @method print gorica
#' @export
print.gorica <- function(x,
                       digits = 3,
                       na.print = "", ...){
  dat <- as.matrix(x$fit)
  #fits <- x$fit
  #dat <- fits[, stats]
  #miss_val <- is.na(dat)
  dat <- formatC(dat, digits = digits, format = "f")
  #dat[miss_val] <- ""
  rownames(dat) <- paste0("H", 1:nrow(dat))
  if(grepl("^H[uc]$", x$hypotheses[length(x$hypotheses)])){
    rownames(dat)[nrow(dat)] <- x$hypotheses[length(x$hypotheses)]
  }

  cat("Informative hypothesis test for an object of class ", class(x$model)[1], ":\n\n", sep = "")
  prmatrix(dat,
           quote = FALSE,
           na.print = na.print)

  if(grepl("^H[uc]$", x$hypotheses[length(x$hypotheses)])){
    cat("\nHypotheses:\n ", paste(rownames(dat)[-nrow(dat)], ": ", x$hypotheses[-nrow(dat)], sep = "", collapse = "\n  "), "\n ",
        paste0(x$hypotheses[length(x$hypotheses)], ": ", c("Unconstrained hypothesis", "Complement of the hypothesis")[match(x$hypotheses[length(x$hypotheses)], c("Hu", "Hc"))], "\n  "))
  } else {
    cat("\nHypotheses:\n ", paste(rownames(dat), ": ", x$hypotheses, sep = "", collapse = "\n  "))
  }

  if(!is.null(x[["warnings"]])){
    warning("Gorica analysis returned the following warnings:\n  ", paste(1:length(x$warnings), ". ", x$warnings, sep = "", collapse = "\n  "))
  }
}


print.ormle <- function(x, digits = max(3, getOption("digits") - 3), ...){
  cat("\n$est\n")
  print(x$est)
  cat("\n")
  cat("\n$restrictedest\n")
  print(x$restrictedest)
  cat("\n")
  invisible(x)
}
