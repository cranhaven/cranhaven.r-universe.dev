#' Conversion between base10 and base30
#'
#' **Note: this function is deprecated; use this function in the `{squids}`
#' package!**
#'
#' The conversion functions from base10 to base30 and vice versa are
#' used by the [rock::generate_uids()] functions.
#'
#' The symbols to represent the 'base 30' system are the 0-9
#' followed by the alphabet without vowels but including the y (see
#' [`squids::squids-package`]).
#'
#' @param x The vector to convert (numeric for `numericToBase30`,
#'          character for `base30toNumeric`).
#'
#' @return The converted vector (numeric for `base30toNumeric`, character for `numericToBase30`).
#' @name base30conversion
#' @rdname base30conversion
#' @examples rock::numericToBase30(
#'   654321
#' );
#' rock::base30toNumeric(
#'   rock::numericToBase30(
#'     654321
#'   )
#' );
#' @export
numericToBase30 <- function(x) {
  .Deprecated(
    new = "numericToBase30",
    package = "squids",
    msg = paste0(
      "The `numericToBase30()` functon in the {rock} package is deprecated. ",
      "Change your code to use the same function in the {squids} package."
    )
  );
  return(squids::numericToBase30(x));
}
