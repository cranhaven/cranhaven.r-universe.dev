#' Identify outliers based on z-score cutoff
#' that are Winsorized by the `winsorZ` function
#'
#' The `winsorZ_find` function is an optional
#' companion to the `winsorZ` function.
#' The `winsorZ` function identifies Z-score outliers and
#' replaces with the next most extreme non-outlier value.
#' The `winsorZ_find` function finds/identifies these
#' Z-score outliers (outliers=1, non-outliers=0).
#' @param x The input variable to check for Z-score outliers.
#' @param zbound The Z-score cutoff (default=3, i.e. outliers are Z>3 | Z<-3).
#' @return Output logical variable of Z-score outliers
#' @export
#' @examples
#' summary(winsorZ_find(psydat$iq))
#' \dontrun{
#' psydat %>% mutate_at(c("iq", "anxT"), list(out = ~ winsorZ_find(.)))
#' }
#'
winsorZ_find <- function(x,
                         zbound = 3) {
  x <- as.numeric(as.character(x)) # convert to numeric just in case
  z <- scale(x) # create z-scores for vector x
  x[!is.na(z) & (z < zbound & z > (-1 * zbound))] <- 0
  x[!is.na(z) & (z > zbound | z < (-1 * zbound))] <- 1
  return(as.factor(x))
}
