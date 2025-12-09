### * TODO

# Clean-up this file

### * None of the functions in this file is exported

### * group2string()

#' Convert a group entry to a string
#'
#' @param x A vector describing a group. Can be NULL.
#'
#' @return A string.
#'
#' @keywords internal
#' @noRd

group2string <- function(x) {
    if (is.null(x)) {
        return("NULL")
    }
    n <- names(x)
    s <- paste(paste(n, x, sep = "="), collapse = "; ")
    return(s)
}

### * Alias for tidy_mcmc()

#' @keywords internal
#' @noRd

tidy_mcmc_list <- function(...) {
    tidy_mcmc(...)
}
