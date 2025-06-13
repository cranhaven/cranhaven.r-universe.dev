#' Repeat a data frame n number of times and return as a single data frame
#' @noRd

df_repeat <- function(x, n) {
    do.call('rbind', replicate(n, x, simplify = FALSE))
}

#' Remove b-spline polynomial nameing
#' @noRd

bs_stripper <- function(x) {
    base_names <- gsub('^bs\\(', '', x)
    base_names <- gsub('\\)[1-9]$', '', base_names)
    base_names <- gsub(', degree = [1-9]', '', base_names)
    base_names <- unique(base_names)
    return(base_names)
}