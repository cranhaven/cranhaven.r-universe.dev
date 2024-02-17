#' Calculate change in richness resulting from a percent reduction in flow
#' @description Calculates absolute or percent richness change from streamflow reduction
#' @param stats A dataframe of ELF statistics
#' @param pctchg Decrease in flow as a percent (e.g. 10 equals 10 percent reduction in flow).
#' @param xval x-axis value for assessing percent change in richness. When supplied, the function will calculate percent change in richness at a specific stream size (e.g. 50 equals a stream size with mean annual flow of 50 cfs).
#' @return Richness change value is returned
#' @export richness_change
#' @examples
#' \donttest{
#' # We don't run this example by R CMD check, because it takes >10s
#'
#' watershed.df <- elfdata(watershed.code = '0208020104', ichthy.localpath = tempdir())
#' breakpt <- 500
#' elf <- elfgen(
#'    "watershed.df" = watershed.df,
#'    "quantile" = 0.95,
#'    "breakpt" = breakpt,
#'    "xlabel" = "Mean Annual Flow (ft3/s)",
#'    "ylabel" = "Fish Species Richness"
#'    )
#' # Calculate absolute richness change
#' richness_change(elf$stats, "pctchg" = 10)
#' # Calculate percent richness change at a specific stream size
#' richness_change(elf$stats, "pctchg" = 10, "xval" = 50)
#' }
richness_change <- function(stats, pctchg, xval = FALSE) {
  m <- stats$m
  b <- stats$b
  pctchg <- pctchg * 0.01

  richness.loss <- m * (log(1 / (1 - pctchg)))
  richness.loss <- -richness.loss #ENSURE A NEGATIVE VALUE IS RETURNED FOR RICHNESS LOSS

  #print(xval)
  if (missing(xval) == FALSE){
    richness.change.percent <- richness.loss / ((m * log(xval)) + b)
    richness.change.percent <- richness.change.percent * 100
    richness.change <- richness.change.percent
    #message(paste("Percent Richness Change at x = ",xval,": ",richness.change,sep = ''))
  } else {
    richness.change <- richness.loss
    #message(paste("Absolute Richness Change: ",richness.change,sep = ''))
  }

  return(richness.change)
}
