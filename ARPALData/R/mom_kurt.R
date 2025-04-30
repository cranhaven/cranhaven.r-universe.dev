#' @keywords internal
#' @noRd

mom_kurt <- function(x, na.rm = FALSE) {
  if (is.matrix(x)) {
    apply(x, 2, FUN = function(x) {
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      (sum((x - mean(x))^4)/n)/(sum((x - mean(x))^2)/n)^(4/2)
      })
  } else if (is.vector(x)) {
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    n <- length(x)
    (sum((x - mean(x))^4)/n)/(sum((x - mean(x))^2)/n)^(4/2)
  } else if (is.data.frame(x)) {
    sapply(X = x, FUN = function(x) {
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      (sum((x - mean(x))^4)/n)/(sum((x - mean(x))^2)/n)^(4/2)
    })
  }
}
