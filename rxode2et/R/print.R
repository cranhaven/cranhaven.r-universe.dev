.getBoundRemember <- NULL
.getBound <- function(x, parent = parent.frame(2)) {
  ## nocov start
  if (!is.null(.getBoundRemember)) return(.getBoundRemember)
  bound <- do.call("c", lapply(ls(globalenv()), function(cur) {
    if (identical(parent[[cur]], x)) {
      return(cur)
    }
    return(NULL)
  }))
  if (length(bound) > 1) bound <- bound[1]
  if (length(bound) == 0) {
    bound <- do.call("c", lapply(ls(parent), function(cur) {
      if (identical(parent[[cur]], x)) {
        return(cur)
      }
      return(NULL)
    }))
    if (length(bound) > 1) bound <- bound[1]
    if (length(bound) == 0) {
      bound <- ""
    }
  }
  return(bound)
  ## nocov end
}


#' @export
print.rxEtTran <- function(x, ...) {
  print(as.data.frame(x))
  .cls <- class(x)
  .lst <- attr(.cls, ".rxode2.lst")
  cat("\nCovariates (non time-varying):\n")
  print(.lst$cov1)
  cat("\nCompartment translation:\n")
  print(data.frame(
    "Compartment Name" = .lst$cmtInfo,
    "Compartment Number" = seq_along(.lst$cmtInfo),
    check.names = FALSE
  ))
}

#' @export
print.rxHidden <- function(x, ...) {
  cat("\r")
}

#' @rdname rxEvid
#' @export
print.rxEvid <- function(x, ...) {
  cat(paste(.colorFmt.rxEvid(x), collapse = "\n"), "\n")
  return(invisible(x))
}

#' @export
print.rxRateDur <- function(x, ...) {
  cat(paste(.colorFmt.rxRateDur(x), collapse = "\n"), "\n")
  return(invisible(x))
}

.h2 <- function(x) {
  cli::cli_text(crayon::bold(paste0(cli::symbol$line, cli::symbol$line, " ", x, " ", cli::symbol$line, cli::symbol$line)))
}



#' @export
print.rxEt <- function(x, ...) {
  if (.isRxEt(x)) {
    bound <- .getBound(x, parent.frame(2))
    .et1 <- paste0("EventTable with ", x$nobs + x$ndose, " records")
    .et2 <- NULL
    .units <- x$.units
    .maxId <- length(x$IDs)
    if (.maxId != 1) {
      .et2 <- sprintf("   %s individuals", .maxId)
    }
    .et3 <- sprintf(
      "   %s dosing records (see %s$%s(); add with %s or %s)",
      x$ndose, bound, "get.dosing", "add.dosing", "et"
    )
    .et4 <- sprintf(
      "   %s observation times (see %s$%s(); add with %s or %s)",
      x$nobs, bound, "get.sampling", "add.sampling",
      "et"
    )
    .et5 <- NULL
    if (x$show["addl"]) {
      .et5 <- sprintf(
        "   multiple doses in `addl` columns, expand with %s$%s(); or %s(%s)",
        bound, "expand", "etExpand", bound
      )
    }
    .et <- c(.et2, .et3, .et4, .et5)
    .df <- data.frame(et = .et, stringsAsFactors = FALSE)
    names(.df) <- .et1
    class(.df) <- c(
      sprintf("EventTable Info: %s", bound),
      "paged_df", "data.frame"
    )
    .out <- utils::capture.output({
      print(.df)
    })
    .nb <- TRUE
    if (length(.out) > 0) {
      .nb <- FALSE
      cat(cli::cli_format_method({
        .h2(.et1)
        cli::cli_text(sprintf(
          "   %s dosing records (see %s$%s(); add with %s or %s)\n",
          x$ndose, crayon::yellow(bound), crayon::blue("get.dosing"),
          crayon::blue("add.dosing"), crayon::blue("et")))
        cli::cli_text(sprintf(
          "   %s observation times (see %s$%s(); add with %s or %s)\n",
          x$nobs, crayon::yellow(bound), crayon::blue("get.sampling"),
          crayon::blue("add.sampling"), crayon::blue("et")))
        if (x$show["addl"]) {
          cli::cli_text(sprintf(
            "   multiple doses in `addl` columns, expand with %s$%s(); or %s(%s)\n",
            crayon::yellow(bound), crayon::blue("expand"),
            crayon::blue("etExpand"), crayon::yellow(bound)
          ))
        }
      }), sep = "\n")


    }
    if (x$nobs != 0 || x$ndose != 0) {
      if (!.nb) {
        cat(cli::cli_format_method({
          .h2(paste0("First part of ", crayon::yellow(bound), ":"))
        }), sep = "\n")
      }
      print(tibble::as_tibble(data.frame(.etAddCls(x))))
    }
    invisible(x)
  } else {
    print.data.frame(x)
  }
}
