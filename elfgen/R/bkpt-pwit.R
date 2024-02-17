#' Identify breakpoint location with PWIT
#'
#' See:
#'  Lemoine, N. 2012. "R for Ecologists: Putting Together a Piecewise Regression." https://www.r-bloggers.com/r-for-ecologists-putting-together-a-piecewise-regression/
#'  The R Book, Second Edition. Michael J. Crawley. 2013 John Wiley & Sons, Ltd. Published 2013 by John Wiley & Sons, Ltd.
#'
#' @description This applies the Piecewise Iterative elfgen method. This approach uses an iterative algorithm to identify shifts in the relation between maximum richness and stream size. A user specifies a "quantile" for isolating an upper subset of the data. A user also identifies a bounding range between two x-values ("blo" = "bound low", "bhi" = "bound high") in which the upper subest of data is believed to contain a breakpoint. (Note: Small datasets may not contain a breakpoint)
#' @param watershed.df A dataframe of sites with ecological and hydrologic data
#' @param quantile Specified value for the quantile of interest - 0.95 refers to the 95th percentile
#' @param blo A "bound low" value, or the lower bound of the piecewise range
#' @param bhi A "bound high" value, or the upper bound of the piecewise range
#' @return Breakpoint value is returned
#' @import quantreg
#' @import stats
#' @export bkpt_pwit
#' @examples
#' \donttest{
#' # We don't run this example by R CMD check, because it takes >10s
#'
#' watershed.df <- elfdata(watershed.code = '0208020104', ichthy.localpath = tempdir())
#' bkpt_pwit(watershed.df,0.85,100,300)
#' }
bkpt_pwit <- function(watershed.df,quantile,blo,bhi) {

  watershed.df.raw <- watershed.df

  #RENAME COLUMNS TO HAVE GENERIC NAMES
  colnames(watershed.df)[1] <- "x_var"
  colnames(watershed.df)[2] <- "y_var"
  colnames(watershed.df)[3] <- "watershed"

  # stop if no quantile parameter supplied
  if(missing(quantile)) {
    stop("Missing quantile parameter")
  }

  # default blo if none provided
  if(missing(blo)) {
    blo <- 0
  }

  upper.quant.data <- rq(y_var ~ log(x_var),data = watershed.df, tau = quantile)
  newy <- c(log(watershed.df$x_var)*coef(upper.quant.data)[2]+coef(upper.quant.data)[1])
  upper.quant <- subset(watershed.df, watershed.df$y_var > newy)

  x <- upper.quant$x_var
  y <- upper.quant$y_var

  #set initial guess range
  breaks <- x[which(x >= blo & x <= bhi)]
  as.numeric(breaks)

  #This is necessary in case no breaks are found
  if(length(breaks) != 0) {
    #Needed in case pwit function only locates a single break in the data
    if(length(breaks) == 1) {
      breakpt <- breaks
    }else{

      mse <- as.numeric(length(breaks))

      for(n in 1:length(breaks)){
        piecewise <- lm(y ~ log(x)*(x < breaks[n]) + log(x)*(x >= breaks[n]))
        mse[n] <- summary(piecewise)[6]
      }
      mse <- as.numeric(mse)
      #remove any breaks that are NaN
      mse[is.na(mse)] <- 100000
      breakpt <- breaks[which(mse==min(mse))]
      breakpt <- breakpt[1]
    } #end of breaks == 1 loop

    #message(paste("breakpoint identified at ",breakpt,sep = ''))

  } else {
    breakpt <- "none identified"
    stop(paste("No breakpoint identified using this set of inputs for ", watershed.df$watershed[1],
               "\n  ... Try using a smaller quantile or a wider bounding range",
               "\n  ... If still unsuccessful, a larger dataset may be required",sep=''))
  }
  return(breakpt)
}
