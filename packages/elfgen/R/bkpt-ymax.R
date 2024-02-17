#' Identify breakpoint location with Ymax
#' @description This applies the Ymax elfgen method. This approach treats the maximum observed species richness value as the breakpoint. This function begins by locating the point with the highest y-value in the full dataset, then utilizing the associated x-value as the breakpoint.
#' @param watershed.df A dataframe of sites with ecological and hydrologic data
#' @return Breakpoint value is returned
#' @export bkpt_ymax
#' @examples
#' \donttest{
#' # We don't run this example by R CMD check, because it takes >10s
#'
#' watershed.df <- elfdata(watershed.code = '0208020104', ichthy.localpath = tempdir())
#' bkpt_ymax(watershed.df)
#' }
bkpt_ymax <- function(watershed.df) {

  watershed.df.raw <- watershed.df

  #RENAME COLUMNS TO HAVE GENERIC NAMES
  colnames(watershed.df)[1] <- "x_var"
  colnames(watershed.df)[2] <- "y_var"
  colnames(watershed.df)[3] <- "watershed"

  x <- watershed.df$x_var
  y <- watershed.df$y_var

  ymax <- max(y) #finds the max y-value
  x.ymax <- subset(watershed.df, watershed.df$y_var == ymax)
  breakpt <- min(x.ymax$x_var)

  #message(paste("Breakpoint identified at ",breakpt,sep = ''))

  return(breakpt)
}
