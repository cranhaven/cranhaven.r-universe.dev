### * None of the functions in this file is exported

### * describe_z_eta()

#' Print a message describing the role of eta or zeta in a distribution family
#'
#' @param param_name Name of the parameter (e.g. "eta" or "zeta").
#' @param family Family string.
#' @param prefix,suffix Strings appended to the message.
#'
#' @keywords internal
#' @noRd

describe_z_eta <- function(param_name, family, prefix = "  (", suffix = ")") {
    msg <- list(
        "gamma_cv" = " is the coefficient of variation of gamma distributions.",
        "normal_cv" = " is the coefficient of variation of normal distributions.",
        "normal_sd" = " is the standard deviation of normal distributions.",
        "beta_phi" = " is the precision (phi) of beta distributions.")
    if (!family %in% names(msg)) {
        stop("The provided value for the family argument is not allowed.")
    }
    message(prefix, param_name, msg[[family]], suffix, sep = "")
}

### * valid_prior_tbl()

#' Test if the input is a valid prior tibble
#'
#' @param x Some input to test.
#'
#' @return Boolean.
#'
#' @examples
#' valid_prior_tbl(priors(aquarium_mod))
#' valid_prior_tbl(mtcars)
#'
#' @keywords internal
#' @noRd

valid_prior_tbl <- function(x) {
    # Is tibble?
    if (!is(x, "tbl_df")) return(FALSE)
    # Has at least columns "in_model" and "prior"?
    if (!all(c("in_model", "prior") %in% colnames(x))) return(FALSE)
    # "in_model" is a character vector?
    if (!is(x[["in_model"]], "character")) return(FALSE)
    # "prior" is a list?
    if (!is(x[["prior"]], "list")) return(FALSE)
    # Are all the entries in the "prior" column NULL or priors?
    non_null <- x[["prior"]][!is.null(x[["prior"]])]
    if (!all(sapply(non_null, is, "prior"))) return(FALSE)
    # End of tests
    return(TRUE)
}
