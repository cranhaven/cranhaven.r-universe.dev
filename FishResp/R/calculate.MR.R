#' Calculation of Metabolic Rate
#'
#' The function is used to calculate and plot background respiration, absolute and mass-specific metabolic rates.
#'
#' @usage
#' calculate.MR(slope.data, density = 1000,
#'              plot.BR = TRUE,
#'              plot.MR.abs = TRUE,
#'              plot.MR.mass = TRUE)
#'
#' @param slope.data  a data frame obtained by using the function \code{\link{extract.slope}}
#' @param density  numeric: the density of an animal body (\eqn{kg/m^{3}})
#' @param plot.BR  logical: if TRUE, the graph of background respiration rate is plotted
#' @param plot.MR.abs  logical: if TRUE, the graph of absolute metabolic rate is plotted
#' @param plot.MR.mass  logical: if TRUE, the graph of mass-specific metabolic rate is plotted
#'
#' @return The function returns a data frame with calculated background respiration, absolute and mass-specific metabolic rates. The data frame is used in the function \code{\link{export.MR}}.
#'
#' @importFrom lattice xyplot
#' @importFrom grDevices dev.new
#' @importFrom graphics abline legend par plot
#' @importFrom stats coef lm predict.lm
#' @importFrom utils head read.table tail write.table
#'
#' @examples
#' # if the data have been already loaded to R,
#' # skip the first two lines of the code:
#' data(SMR.slope)
#' data(AMR.slope)
#'
#' SMR <- calculate.MR(SMR.slope,
#'                     density = 1000,
#'                     plot.BR = TRUE,
#'                     plot.MR.abs = TRUE,
#'                     plot.MR.mass = TRUE)
#'
#' AMR <- calculate.MR(AMR.slope,
#'                     density = 1000,
#'                     plot.BR = TRUE,
#'                     plot.MR.abs = TRUE,
#'                     plot.MR.mass = TRUE)
#'
#' @export

calculate.MR  <- function(slope.data, density = 1000, plot.BR = TRUE,
                          plot.MR.abs = TRUE, plot.MR.mass = TRUE){

  BW = slope.data$Mass/1000 # convert Mass from 'g' to 'kg'
  V = slope.data$Volume/1000 - (BW/(density/1000)) #convert Volume from 'mL' to 'L' and Density from 'kg/M^3' to 'kg/L'

  slope.data$MR.abs.with.BR = -(slope.data$Slope.with.BR*V*3600) #3600 sec = 1 hour

  slope.data$BR = (slope.data$Slope.with.BR - slope.data$Slope)/slope.data$Slope.with.BR*100 # in %
  slope.data$MR.abs = -(slope.data$Slope*V*3600)
  slope.data$MR.mass = -(slope.data$Slope*V*3600/BW)

  a <- xyplot(BR~Temp|Ind, data=slope.data, as.table = T,
              xlab = bquote("Temperature (" ~ C^o ~ ")"), ylab = "Background respiration (%)",
              main = "Percentage rate of background respiration")
  b <- xyplot(MR.abs~Temp|Ind, data=slope.data, as.table = T,
              xlab = bquote("Temperature (" ~ C^o ~ ")"), ylab = bquote("Absolute MR (" ~ .(slope.data$DO.unit[1]) ~ h^-1 ~ ")"),
              main = "Absolute metabolic rate")
  d <- xyplot(MR.mass~Temp|Ind, data=slope.data, as.table = T,
              xlab = bquote("Temperature (" ~ C^o ~ ")"), ylab = bquote("Mass-specific MR (" ~ .(slope.data$DO.unit[1]) ~ kg^-1 ~ h^-1 ~ ")"),
              main = "Mass-specific metabolic rate")

  if (plot.BR == TRUE){
    par(mfrow = c(2, 1), ask = T)
    print(a)
  }

  if (plot.MR.abs == TRUE){
    par(mfrow = c(2, 1), ask = T)
    print(b)
  }

  if (plot.MR.mass == TRUE){
    par(mfrow = c(2, 1), ask = T)
    print(d)
  }
  a <- slope.data$DO.unit[1]
  MR.data <- slope.data[,-12]
  MR.data$DO.unit <- a[1]
  return(MR.data)
}
