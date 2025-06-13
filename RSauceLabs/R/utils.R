#' @export
print.account <- function(x, ...){
  cat("Sauce Labs Account:\n")
  cat("SauceLabs username:", strsplit(x[["request"]]$options$userpwd, ":")[[1]][1], "\n")
}
