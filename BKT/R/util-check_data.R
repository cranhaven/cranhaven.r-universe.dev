check_data <- function(data) {
  if (ncol(data$data) != length(data$resources)) {
    stop("data and resource sizes must match")
  }

  if (!all(data$starts + data$lengths - 1 <= ncol(data$data))) {
    stop("data lengths don't match its shape")
  }
}
