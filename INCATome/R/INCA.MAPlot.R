#' MA Plot for Array data, with optional highlighting of set of genes
#'
#' Plots MA plot for microarray data and highlights sets of genes and/or SpikeIn probes.
#' @param x an RGList object
#' @param wcol an integer specifying the number of the column where Gene Names can be found in the gene annotation table.
#' @param spikeIn logical, TRUE to highlight SpikeIn Probes. Requires input in SpikeFile.
#' @param SpikeFile a data.frame specifying the Spike In probe names if spikeIn=TRUE in a column called "Probe" and the expected relative amounts for each dye, respectively in a "Cy5" and "Cy3" column. For example, a given probe might be expected in a 3:1 ratio thus column "Cy5" would specify 3 and column "Cy3" would specify 1. 
#' @param prefix a character specifying the prefix to be used when saving the plot in a jpeg file.
#' @param highlight a character vector specifying a set of genes of interest to be highlighted in the plot.
#' @return Generates jpeg files of MA plots for each arrays.
#' @examples
#' #Load the INCATome Dataset
#' data(INCATomeData)
#' attach(INCATomeData)
#' INCA.MAPlot(RGdata,8,spikeIn=TRUE,SpikeFile=sdata, highlight=c("ACTB","PABPC1"))
#' @importFrom grDevices dev.off jpeg rainbow rgb
#' @importFrom graphics abline arrows legend points text
#' @importFrom stats cor median model.matrix na.omit sd
#' @importFrom utils write.table
#' @export

INCA.MAPlot <- function(x, wcol, spikeIn=TRUE, SpikeFile, prefix="", highlight=NULL){
  if(class(x) != "RGList"){
    stop("Please supply an RGList as the first argument")
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
  if (spikeIn==T & missing(SpikeFile)){
    stop("The SpikeIn File is missing")
  }
  if(!is.null(highlight)){
    hcol <- rainbow(length(highlight))
  }
  
  
  
  R <- x$R
  G <- x$G
  
  R[R <= 0] <- NA
  G[G <= 0] <- NA
  
  M <- R / G
  A <- (R * G)/2
  
  M <- log(M,2)
  A <- log(A,2)
  
  
  for(i in 1:ncol(x$R)){
    jpeg(paste("MAplot",prefix,i,".jpg", sep=""), quality=100, width=600, height=600)
    plot(x=A[, i], y=M[, i], ylab="M", xlab="A", col=rgb(0,0,0,0.1), pch=19)
    
    #Find the control types
    if (spikeIn==T && !missing(SpikeFile)){
      useSI <- c()
      for(q in 1:length(SpikeFile$Probe)){
        useSI <- c(useSI, grep(paste("^", SpikeFile$Probe[q],"$", sep=""), x$genes[,wcol]))
      }
      points(x=A[useSI,i], y=M[useSI,i], col=rgb(1,1,0,0.3), pch=19)
    }
    
    
    if(!is.null(highlight)){
      for(j in 1:length(highlight)){
        hl <- grep(paste("^",highlight[j],"$", sep=""), x$genes[,wcol])
        if(length(hl) > 0){
          points(x=A[hl,i], y=M[hl,i], col=hcol[j], pch=15)
          text(x=A[hl,i], y=M[hl,i], highlight[j], col=hcol[j], pos=4)
        }
      }
    }
    abline(h=0, col="red")
    legend("topleft", horiz=T, fill=c(rgb(0,0,0,1),  if(spikeIn==T) {rgb(1,1,0,1)} ,if(!is.null(highlight)) {hcol}), 
           legend=c("Data", if(spikeIn==T) {"SpikeIn"},if(!is.null(highlight)) {highlight}), cex=0.6)
    
    dev.off()
  
  }
  
}