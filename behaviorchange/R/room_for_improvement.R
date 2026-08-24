#' @rdname potential_for_change
#'
#' @param varName For internal use.
#'
#' @export
room_for_improvement <- function(x,
                                 increasesAreImprovements = TRUE,
                                 sampleLevel = FALSE,
                                 minimum = base::min,
                                 maximum = base::max,
                                 center = base::mean,
                                 minimumArgs = list(na.rm=TRUE),
                                 maximumArgs = list(na.rm=TRUE),
                                 centerArgs = list(na.rm=TRUE),
                                 varName = NULL) {

  if (length(dim(x)) == 2) {

    ### If we get a data frame or array, apply for each column

    if (is.array(x)) {
      x <- as.data.frame(x);
    }

    nCols <- ncol(x);

    ### If single number or functions are provided, replicate them to have
    ### the same length as the number of columns for slightly more ease
    ### later on. Note that we wrap them in lists first to enable us to use
    ### `rep` on functions, should those be passed.

    if (length(increasesAreImprovements) == 1) {
      increasesAreImprovements <- rep(increasesAreImprovements, nCols);
    } else if (length(increasesAreImprovements) != nCols) {
      stop("As `increasesAreImprovements`, pass either `TRUE` or `FALSE`, ",
           "or a logical vector with the same length as `x` has columns.");
    }
    if (length(minimum) == 1) {
      minimum <- rep(list(minimum), nCols);
    } else if (length(minimum) != nCols) {
      stop("As `minimum`, pass either a function or a number (or for ",
           "advanced users, a vector of numbers or a list of functions).");
    }
    if (length(maximum) == 1) {
      maximum <- rep(list(maximum), nCols);
    } else if (length(maximum) != nCols) {
      stop("As `maximum`, pass either a function or a number (or for ",
           "advanced users, a vector of numbers or a list of functions).");
    }
    if (length(center) == 1) {
      center <- rep(list(center), nCols);
    } else if (length(center) != nCols) {
      stop("As `center`, pass either a function or a number (or for ",
           "advanced users, a vector of numbers or a list of functions).");
    }

    res <-
      lapply(
         1:nCols,
         function(i) {
           res <-
             room_for_improvement(
               x = x[, i],
               increasesAreImprovements = increasesAreImprovements[[i]],
               sampleLevel = sampleLevel,
               minimum = minimum[[i]],
               maximum = maximum[[i]],
               center = center[[i]],
               minimumArgs = minimumArgs,
               maximumArgs = maximumArgs,
               centerArgs = centerArgs,
               varName = names(x)[[i]]
             );
           return(res);
         }
      );

    res <- as.data.frame(res);
    names(res) <- names(x);
    return(res);

  } else {

    ### x is a vector

    if (!is.numeric(x)) {
      if (is.null(varName)) {
        varName <- deparse(substitute(x));
      }
      stop("You passed a variable (", varName, ") that does not have ",
           "class 'numeric', but instead has class(es) ", vecTxtQ(class(x)),
           ". Please convert the variables (data frame columns) to numeric ",
           "yourself first; I could do this for you, but won't, as this ",
           "process often requires manual inspection to verify the data ",
           "integrity after conversion. You can use for example ",
           "`as.numeric()` for this conversion, or `ufs::convertToNumeric()`.");
    }

    if (is.function(minimum)) {
      lowerBound <-
        do.call(
          minimum,
          args = c(list(x),
                   minimumArgs)
        );
    } else if ((is.numeric(minimum)) &&
               ((length(minimum) == 1) || (length(minimum) == length(x)))) {
      lowerBound <- minimum;
    } else {
      stop("As argument `minimum`, you must pass either a function ",
           "that takes a vector and returns the minimum value; or a numeric ",
           "value that is then used as minimum.");
    }

    if (is.function(maximum)) {
      upperBound <-
        do.call(
          maximum,
          args = c(list(x),
                   maximumArgs)
        );
    } else if ((is.numeric(maximum)) &&
               ((length(maximum) == 1) || (length(maximum) == length(x)))) {
      lowerBound <- maximum;
    } else {
      stop("As argument `maximum`, you must pass either a function ",
           "that takes a vector and returns the maximum value; or a numeric ",
           "value that is then used as maximum.");
    }

    if (sampleLevel) {

      ### Sample-level, so the distance of the center (e.g. mean) to the
      ### upper or lower bound

      if (is.function(center)) {
        statusQuo <-
          do.call(
            center,
            args = c(list(x),
                     centerArgs)
          );
      } else {
        stop("As argument `center`, you must pass a function that ",
             "that takes a vector and returns an estimate of the central ",
             "tendency, such as `base::mean` and `base::median` (or a variation ",
             "on these that you created yourself).");
      }

      if (increasesAreImprovements) {
        return(upperBound - statusQuo);
      } else {
        return(lowerBound - statusQuo);
      }

    } else {
      ### Individual-level, so simple the difference between people's scores
      ### and the upper or lower bound (depending on whether the behavior
      ### is a desirable behavior (higher scores are good) or not
      if (increasesAreImprovements) {
        return(upperBound - x);
      } else {
        return(lowerBound - x);
      }
    }
  }
}