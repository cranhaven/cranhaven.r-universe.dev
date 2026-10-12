#' @title Simple plot for prosper simulation results
#' @seealso \code{\link{summary}} 
#' @description This function draws three figures. i)Numerics: mean number of individual plants of a specified development stage for each simulation cycle,  summing up the genotypes to one number per cycle. ii)Raw counts: same as i) using the results of the repetitions instead of the mean. iii) Mean of the proportion of plants with only alleles for sensitivity, proportion of plants with only alleles for resistance, proportion of plants with mixed alleles for resistance and sensitivity, proportion of R alleles in the population.  
#' @export
# @usage plot(x, y, plot_var = "SB_autumn", ...)
#' @aliases plot

#' @param x \code{prosper}, the result of a prosper simulation.
#' @param y not used.
#' @param plot_var variable, i.e. a column name of the result data.frame, to be plotted. \code{character}. 
#' @param \dots other graphical parameters.

#' @examples
#' data(param.LOLRI)
#' mod_lolri    <- prosper.LOLRI(param.weed=param.LOLRI, area=20,  af = c(0.005, 0.01),
#'                                        duration=3, repetitions=2)
#' plot(mod_lolri)

#' @name plot

if (!isGeneric("plot"))
      setGeneric("plot", function(x, y, ...) standardGeneric("plot")) 
      
#' @rdname plot
setMethod( "plot",
           c(x="prosper", y="missing"),
           function(x,y, plot_var="SB_autumn", ...){
#sinkv <- ifelse(.Platform$OS.type=="windows","NUL" , "/dev/null")
#sink(sinkv)
dat <- summary(x)
#sink()
numerics <- dat$population$numerics
counts <- dat$population$counts



if(length(plot_var) != 1)  stop("You must set one varibale to be plotted as plot_var")
if(!(plot_var %in% names(counts))) stop(paste(plot_var, "is not part of the variable names. Please set plot_var in plot() correctly."))


###---------------------------------------------------
# Raw counts
dev.new()
plotcols <- c(which(names(counts)=="simcycle"), which(names(counts)==plot_var))
plot(counts[,c(plotcols),with=FALSE], pch=seq_along(plotcols[-1]), type="n", main="Raw Counts", xlab="simulation cycle",...)
   points(counts[,c(plotcols),with=FALSE], pch=seq_along(plotcols[-1]), ...)
graphics::axis(1,c(1:max(counts$simcycle)))   

###---------------------------------------------------
#Numerics
dev.new()
plotcols <- c(which(names(numerics)=="simcycle"), grep(paste(plot_var, "_mean", sep=""), names(numerics)))
plot(numerics[,c(plotcols),with=FALSE], type="n", main="Numerics", xlab="simulation cycle", ...)
   points(numerics[,c(plotcols),with=FALSE], pch=seq_along(plotcols[-1]), ...)
   points(numerics[,c(plotcols),with=FALSE], lty=seq_along(plotcols[-1]), type="l", ...)
graphics::axis(1,c(1:max(numerics$simcycle)))
   
###---------------------------------------------------
#Genetics
if(!is.null(dat$genetics)){
   genetics <- dat$genetics   
dev.new()
 plotcols <- c(which(names(genetics)=="simcycle"), grep("m_", names(genetics)))  
 
x_lim <- c(1,max(genetics$simcycle))
y_lim <- c(0,1)

plot(genetics[,c(1, plotcols[2]),with=FALSE], type="n", ylim=y_lim, xlim=x_lim,  xlab="simulation cycle", ylab="proportion", xaxt="n", main="genetics", ...)
points(genetics[,c(1, plotcols[2]),with=FALSE], type="l", ...)
points(genetics[,c(1, plotcols[2]),with=FALSE], type="p", pch=16, ...)

points(genetics[,c(1, plotcols[3]),with=FALSE], lty=2, type="l", ...)
points(genetics[,c(1, plotcols[3]),with=FALSE], type="p",pch=17, ...)

points(genetics[,c(1, plotcols[4]),with=FALSE], lty=3, type="l", ...)
points(genetics[,c(1, plotcols[4]),with=FALSE], type="p",pch=18, ...)

points(genetics[,c(1, plotcols[5]),with=FALSE], lty=4, type="l", ...)
points(genetics[,c(1, plotcols[5]),with=FALSE], type="p",pch=1, ...)

legend(x="topright", legend=names(genetics)[plotcols[-1]], inset=c(0.003,0.003),lty=c(1,2,3,4),pch=c(16,17,18,1),merge=T, col=c("black","black","black","black"))
graphics::axis(1,c(1:max(genetics$simcycle)))
}#END plot genetics 

          
}#END function
)#END Method
