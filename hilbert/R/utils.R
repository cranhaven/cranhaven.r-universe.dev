#nocov start

#' @keywords internal
.extent <- function(x, y) {
    c(xmax = max(x), xmin = min(x), ymax = max(y), ymin = min(y))
}

#nocov end
