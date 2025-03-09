#' Generate utterance identifiers (UIDs)
#'
#' This function generates utterance identifiers.
#'
#' @param x The number of identifiers te generate.
#' @param origin The origin to use when generating the actual
#' identifiers. These identifiers are the present UNIX timestamp
#' (i.e. the number of seconds elapsed since the UNIX epoch,
#' the first of january 1970), accurate to two decimal places
#' (i.e. to centiseconds), converted to the base 30 system using
#' [numericToBase30()]. By default, the present time is used as
#' origin, one one centisecond is added for every identifiers to
#' generate. `origin` can be set to other values to work with
#' different origins (of course, don't use this unless you
#' understand very well what you're doing!).
#'
#' @return A vector of UIDs.
#' @export
#'
#' @examples rock::generate_uids(5);
#'
#' ### Show how UIDs are the converted date/time
#' x <- rock::generate_uids(1);
#' x;
#' x_UID <- gsub(
#'   "\\[\\[uid=(.*)\\]\\]",
#'   "\\1",
#'   x
#' );
#' x_as_nr <- rock::base30toNumeric(x_UID);
#' x_as_timestamp <- x_as_nr / 100;
#' x_as_date <-
#'   as.POSIXct(
#'     x_as_timestamp,
#'     origin = "1970-01-01",
#'     tz = "UTC"
#'   );
#' x_as_date
generate_uids <- function(x,
                          origin=Sys.time()) {

  uidPrefix <- rock::opts$get(uidPrefix);
  codeDelimiters <- rock::opts$get(codeDelimiters);

  timeNrString <- as.character(round(as.numeric(origin) * 100, 0));
  timeNrs <-
    as.numeric(timeNrString) + (0:(x-1));
  res <-
    unlist(lapply(timeNrs,
                  numericToBase30));
  return(paste0(codeDelimiters[1], uidPrefix, res, codeDelimiters[2]));
}
