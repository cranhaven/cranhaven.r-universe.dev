#' Extract the UIDs (or SQUIDs) from a vector
#'
#' @param x The vector
#' @param returnSQUIDs WHether to return the UIDs or the SQUIDs.
#'
#' @returns A vector with (SQ)UIDs
#' @export
#'
#' @examples exampleText <- c(
#'   "Lorem ipsum dolor sit amet, consectetur",
#'   "adipiscing elit. Nunc non commodo ex,",
#'   "ac varius mi. Praesent feugiat nunc",
#'   "eget urna euismod lobortis. Sed",
#'   "hendrerit suscipit nisl, ac tempus",
#'   "magna porta et. Quisque libero massa,",
#'   "tempus vel tristique lacinia, tristique",
#'   "in nulla. Nam cursus enim dui, non",
#'   "ornare est tempor eu. Vivamus et massa",
#'   "consectetur, tristique magna eget,",
#'   "viverra elit."
#' );
#'
#' withUIDs <-
#'   rock::prepend_ids_to_source(
#'     exampleText
#'   );
#'
#' rock::extract_uids(
#'   withUIDs
#' );
extract_uids <- function(x,
                         returnSQUIDs = FALSE) {

  uidPrefix <- rock::opts$get(uidPrefix);
  utteranceMarker <- rock::opts$get(utteranceMarker);

  if (returnSQUIDs) {
    regexToMatch <-
      paste0("\\[\\[", uidPrefix, "([0123456789bcdfghjklmnpqrstwxyz]{8})\\]\\].*");
  } else {
    regexToMatch <-
      paste0("(\\[\\[", uidPrefix, "[0123456789bcdfghjklmnpqrstwxyz]{8}\\]\\]).*");
  }

  res <-
    gsub(
      regexToMatch,
      "\\1",
      regmatches(
        x,
        regexpr(
          regexToMatch,
          x,
          perl = TRUE
        )
      )
    );

  return(res);

}
