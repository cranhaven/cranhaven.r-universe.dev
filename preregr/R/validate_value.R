#' Validate a value
#'
#' This function validates a value. Before validation, it checks
#' the validation expression and optionally performs replacements, where the
#' replacement strings (delimited by the validation replacement delimiters,
#' by default, `{{` and `}}`) are
#' replaced by the first valid corresponding value from the
#' `replacementSources` (working through those sources consecutively, i.e.
#' only looking in the second one of the first one doesn't contain a valid
#' value for the relevant replacement string, valid being defined as a value
#' that is not NULL or NA and has, after being trimmed of whitespace, a
#' nonzero length).
#'
#' To change the validation replacement delimiters, use
#' `preregr::opts$set(validation_replacementDelimiters = c("{{", "}}"));`.
#'
#' @param VALUE The value to validate.
#' @param validations The validations, a vector or list that will be
#' consecutively searched for the first valid value (valid being defined as
#' a value that is not NULL or NA and has, after being trimmed of whitespace,
#' a nonzero length). That value has to be either as an R expression, or a
#' character value (i.e. a length 1 character vector) that is a valid R
#' expression, optionally after having performed the specified replacements.
#' @param replacementSources A list of named lists (or 1-row data frames) that
#' will be searched, consecutively, for values to replace the replacement
#' strings with.
#' @param errorMessages The errormessage to return if validation fails, of which
#' the first valid one will be returned (valid being defined as a value that is
#' not NULL or NA and has, after being trimmed of whitespace, a nonzero length).
#' @param convert_bipiped Whether to first run [bipiped_value_to_vector()] on
#' the replacment values.
#'
#' @return The message resulting from the validation (i.e. an error or "").
#' @export
#'
#' @examples ### Set some validation variables
#' validationStatement <-
#'   paste(
#'     "is.na(VALUE) ||",
#'     "(VALUE %in% {{validValues}}) ||",
#'     "(VALUE %in% {{testField}})"
#'   );
#' replacementSources <-
#'   list(
#'     list(validValues = '"testValue" || "anotherValue"'),
#'     list(testField = "Yet another testvalue")
#'   );
#' errorMessages <-
#'   "No valid test value passed!";
#'
#' ### Run a passing validation
#' preregr::validate_value(
#'   "testValue",
#'   validations = validationStatement,
#'   replacementSources = replacementSources,
#'   errorMessages = errorMessages
#' );
#'
#'
#' ### Run a failing validation
#' preregr::validate_value(
#'   "A testvalue that won't pass",
#'   validations = validationStatement,
#'   replacementSources = replacementSources,
#'   errorMessages = errorMessages
#' );
validate_value <- function(VALUE,
                           validations,
                           replacementSources,
                           errorMessages,
                           convert_bipiped = TRUE) {

  validation_replacementDelimiters <-
    preregr::opts$get(
      "validation_replacementDelimiters"
    );

  validation <- first_valid_value(validations);

  if (is.character(validation)) {

    validation_replacementDelimiters <-
      gsub("\\{", "\\\\{", validation_replacementDelimiters);
    validation_replacementDelimiters <-
      gsub("\\}", "\\\\}", validation_replacementDelimiters);

    regex <- paste0(validation_replacementDelimiters[1],
                    "([a-zA-Z0-9_]*)",
                    validation_replacementDelimiters[2]);

    matchObject <- gregexpr(regex, validation);

    replacementColumns <- regmatches(validation, matchObject)[[1]];

    replacementColumns <- gsub(regex, "\\1", replacementColumns);

    replacementValues <- c();
    for (i in seq_along(replacementColumns)) {
      replacementValues[i] <-
        first_valid_value(replacementSources,
                          selectName = replacementColumns[i]);
    }

    if (convert_bipiped) {
      replacementValues <-
        unlist(
          lapply(
            bipiped_values_to_vector(replacementValues),
            function(vec) {
              if (length(vec) == 1) {
                return(paste0('"', vec, '"'));
              } else {
                return(
                  paste0(
                    'c("',
                    paste0(vec, collapse='", "'),
                    '")'
                  )
                );
              }
            }
          )
        );
    }

    if (!is.null(replacementValues)) {
      regmatches(validation, matchObject) <- list(replacementValues);
    }

    validation <- as.expression(validation);

  }

  validationOutcome <- eval(parse(text=validation));

  if (is.null(validationOutcome)) {
    errorMsg <- "Validation code evaluated to NULL.";
  } else if (is.na(validationOutcome)) {
    errorMsg <- "Validation code evaluated to NA.";
  } else if (is.logical(validationOutcome) && !validationOutcome) {
    errorMsg <- first_valid_value(errorMessages);
    if (is.null(errorMsg)) {
      errorMsg <- "Validation failed, but no corresponding error message defined.";
    }
  } else {
    errorMsg <- "";
  }

  return(errorMsg);

}
