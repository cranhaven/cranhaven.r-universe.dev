#' DyeSwap Method for two-color microarray data
#'
#' Performs a dyeswap correction by an averaging method for two-color microarray data.
#' @param x an RGList object
#' @param dsvect an integer vector specifying dyeswapped microarrays. Needs to be of same length as number of arrays contained in the RGList object. Labelling should start from 1 and associates dyeswapped microarray with "-i". 
#' @return a new RGList object containing the dyeswapped array data.
#' @examples
#' #Load the INCATome Dataset
#' data(INCATomeData)
#' attach(INCATomeData)
#' ds=INCA.DyeSwap(RGdataNM,c(1,2,3,4,5,6,-1,-2,-3,-4,-5,-6))
#' @export

INCA.DyeSwap<-function(x,dsvect){
  
  if(class(x) != "RGList"){
    stop("Please supply an RGList with agilent data as the first argument")
  }
  if (missing(dsvect)){
    stop("The DyeSwap vector is missing")
  }
  if(length(dsvect)-length(x$targets$FileName)!=0){
    stop("The DyeSwap vector is of different dimension compared to target file")
  }
  
ds <- x

ds$Rb <- c()
ds$Gb <- c()
ds$R <- c()
ds$G <- c()
fn <- c()

ts.data <- new("data.frame")

col <- 1

#Cycle through the experiments
for(a in 1:(length(dsvect)/2)){
    #Where the replicates can be found
    above <- which(dsvect == a)
    below <- which(dsvect == -a)
    
    if(length(above) < 1 | length(below) < 1){
      print(paste("There is no dyeswap for array ",dsvect[a], sep=""))
    }else{
      R <- (x$R[, above] + x$G[, below]) * 0.5
      G <- (x$G[, above] + x$R[, below]) * 0.5
      
      if(col == 1){
        ds$R <- R
        ds$G <- G
        
      }else{
        ds$R <- cbind(ds$R, R)
        ds$G <- cbind(ds$G, G)
      }
      
      col <- col + 1
      print(paste("Dyeswapped Line",above,"and Line",below,"from Targets file"),sep=" ")
    }
  }
return(ds)
}
