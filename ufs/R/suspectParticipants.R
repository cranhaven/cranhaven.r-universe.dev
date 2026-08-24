#' Selects suspect participants from a `carelessObject`
#'
#' This function is a wrapper for the [carelessObject()]
#' function, which wraps a number of functions from
#' the `careless` package. Normally, you'd probably
#' call `carelessReport` which calls this function
#' to generate a report of suspect participants.
#'
#' @param carelessObject The result of the call to
#' [carelessObject()].
#' @param nFlags The number of flags required to be considered
#' suspect.
#' @param digits The number of digits to round to.
#' @param missingSymbol How to represent missing values.
#'
#' @return A logical vector.
#' @export
#'
#' @examples suspectParticipants(carelessObject(mtcars),
#'                     nFlags = 2);
suspectParticipants <- function(carelessObject,
                                nFlags = 1,
                                digits = 2,
                                missingSymbol = "Missing") {

  if (!requireNamespace("careless", quietly = TRUE)) {
    message("Package \"careless\" needed for this function to work. You can install it using:\n\n",
            "  install.packages('careless');\n");
    return(invisible(FALSE));
  }

  if (inherits(carelessObject, "carelessObject")) {
    carelessObject <- unclass(carelessObject);
  } else {
    stop("The argument you pass must be the result of a call to `carelessObject`!");
  }

  flaggedCols <-
    grep('_flagged', names(carelessObject$dat), value=TRUE);

  carelessObject$dat$nFlags <-
    rowSums(carelessObject$dat[, flaggedCols], na.rm=TRUE);

  for (currentMetric in carelessObject$convenience$metrics) {
    carelessObject$dat[, paste0(currentMetric, "_chr")] <-
      ifelse(
        is.na(
          carelessObject$dat[, currentMetric]
        ),
        missingSymbol,
        paste0(
          round(carelessObject$dat[, currentMetric], digits),
          ifelse(
            carelessObject$dat[, paste0(currentMetric, "_flagged")],
            "*",
            " "
          )
        )
      );
  }

  return(carelessObject$dat[carelessObject$dat$nFlags >= nFlags, ]);
}
