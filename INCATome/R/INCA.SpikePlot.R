#' Linearity Plot for Spike In Probes
#'
#' Plots a linearity plot for Spike In probes for microarray data.
#' @param x an RGList object
#' @param SpikeFile a data.frame specifying the Spike In probe names in a column called "Probe" and the expected relative amounts for each dye, respectively in a "Cy5" and "Cy3" column. For example, a given probe might be expected in a 3:1 ratio thus column "Cy5" would specify 3 and column "Cy3" would specify 1. 
#' @param wcol an integer specifying the number of the column where Gene Names can be found in the gene annotation table.
#' @param base an integer specifying the log base. Default is 2.
#' @return Generates jpeg files of SpikeIn Linearity plots for each arrays.
#' @examples
#' #Load the INCATome Dataset
#' data(INCATomeData)
#' attach(INCATomeData)
#' INCA.SpikePlot(RGdata,sdata,8)
#' @importFrom grDevices dev.off jpeg rainbow rgb
#' @importFrom graphics abline arrows legend points text
#' @importFrom stats cor median model.matrix na.omit sd
#' @importFrom utils write.table
#' @export

INCA.SpikePlot <- function(x, SpikeFile, wcol, base=2){
  if(class(x) != "RGList"){
    stop("The first argument is not an RGList")
  }
  if (missing(SpikeFile)){
    stop("The SpikeIn File is missing")
  }
  if(missing(wcol)){
    src=c("agilent","arrayvision","bluefuse","genepix",
          "imagene9","quantarray","scanarrayexpress","smd")
    colh=c("GeneName","ID","NAME","Name","Gene ID","Name","","Gene Name")
    m=match(x$source,src)
    wcol=which(colnames(x$genes)==colh[m])
    if (length(wcol)==0){
      stop("Unable to assign column for Annotation. Please supply a working column for gene Annotation")
    }
  }
  
  Spike.desc <- SpikeFile$Probe
  Spike.R <- SpikeFile$Cy5
  Spike.G <- SpikeFile$Cy3
  
  Spike.ratio <- Spike.R/Spike.G
  
  Spike.expec <- log(1/Spike.ratio, base)
  
  for(i in 1:ncol(x$R)){
    means <- mat.or.vec(length(Spike.desc), 1)
    sds <- mat.or.vec(length(Spike.desc), 1)
    
    for(sp in 1:length(Spike.desc)){
      use <- grep(Spike.desc[sp], x$genes[,wcol])
      
      ratios <- log(x$R[use, i] / x$G[use, i], base)
      
      means[sp] <- mean(ratios[is.finite(ratios)], na.rm=T)
      sds[sp] <- sd(ratios[is.finite(ratios)], na.rm=T)
    }
    jpeg(paste("SpikeInPlot",i,".jpg",sep=""), width=400, height=400, quality=100)
    plot(Spike.expec, means, ylab="Observed", xlab="Expected", main=paste("Spike-in Linearity:", round(cor(1/means, Spike.expec),2), sep=""), col="green")
    arrows(Spike.expec, means, Spike.expec, (means)+sds, angle=90, code=3, length=0.05)
    arrows(Spike.expec, means, Spike.expec, (means)-sds, angle=90, code=3, length=0.05)
    abline(0, 1, col="red")
    dev.off()
  }
}