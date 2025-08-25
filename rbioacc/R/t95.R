#' Return the time at 95% depuration of the parent component
#'
#' @param fit An object of class \code{fitTK}
#' 
#' @return a numeric object
#'
#' @export
#'
t95 <- function(fit){
  medians <- quantile_table(fit, 0.5)
  out <- c(medians[grepl("^km", unname(unlist(medians[2]))),]$`50%`, medians[c("keg","kee"),]$`50%`)
  return(-log(0.05)/sum(out[!is.na(out)]))
}