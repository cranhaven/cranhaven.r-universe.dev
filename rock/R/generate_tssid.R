#' Generate a TSSID
#'
#' A TSSID is a Timestamped Source Identifier. It is a practically
#' unique identifier for a source, conventionally the time and date
#' the source was produced (e.g. when the data were collected, for example
#' the time and date of an interview) in the UTC timezone and in ISO 8601
#' standard format.
#'
#' @param x The date and time to use.
#' @param addDelimiters If `TRUE`, add the delimiters (by default, `[[` and
#' `]]`).
#'
#' @inheritParams prepend_tssid_to_source
#' @returns The tssid
#' @export
#'
#' @examples rock::generate_tssid();
generate_tssid <- function(x = Sys.time(),
                           addDelimiters = FALSE,
                           designationSymbol = "=") {

  res <-
    format(
      as.POSIXct(x, tz = "UTC"),
      "%Y%m%dT%H%MZ"
    );

  if (addDelimiters) {

    codeDelimiters <- rock::opts$get(codeDelimiters);

    res <-
      paste0(
        codeDelimiters[1],
        "tssid",
        designationSymbol,
        res,
        codeDelimiters[2]
      );

  }

  return(
    res
  );

}
