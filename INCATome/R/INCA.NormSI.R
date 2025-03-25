#' INCATome Normalisation by Spike In Probes
#'
#' Performs the INCATome normalisation using invariance of Spike In probes for microarray data.
#' @param x an RGList object
#' @param SpikeFile a data.frame specifying the Spike In probe names in a column called "Probe" and the expected relative amounts for each dye, respectively in a "Cy5" and "Cy3" column. For example, a given probe might be expected in a 3:1 ratio thus column "Cy5" would specify 3 and column "Cy3" would specify 1. 
#' @param wcol an integer specifying the number of the column where Gene Names can be found in the gene annotation table.
#' @param base an integer specifying the log base. Default is 2.
#' @param mva logical, TRUE to plot MA plots before and after normalisation for each array.
#' @param highlight a character vector specifying a set of genes of interest. These will be highlighted in the graphical representations.
#' @return A new RGList object containing the normalised array data. Additionally, if mva is TRUE, MA plots before and after normalisations will be generated for each arrays.
#' @examples
#' #Load the INCATome Dataset
#' data(INCATomeData)
#' attach(INCATomeData)
#' dc=INCA.NormSI(RGdataBG,sdata,8,highlight=c("ACTB","PABPC1"))
#' @importFrom grDevices dev.off jpeg rainbow rgb
#' @importFrom graphics abline arrows legend points text
#' @importFrom stats cor median model.matrix na.omit sd
#' @importFrom utils write.table
#' @export

INCA.NormSI <- function(x, SpikeFile, wcol, base = 2, mva=TRUE, highlight=NULL){
  
  
  if(class(x) != "RGList"){
    stop("Please supply an RGList with agilent data as the first argument")
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
  
  ord <- order(Spike.expec)
  Spike.expec <- Spike.expec[ord]
  Spike.ratio <- Spike.ratio[ord]
  Spike.desc <- Spike.desc[ord]
  
  
  prermsd <- mat.or.vec(ncol(x$R), 1)
  postrmsd <- mat.or.vec(ncol(x$R), 1)
  
  M <- mat.or.vec(nrow(x$R), ncol(x$R))
  A <- mat.or.vec(nrow(x$R), ncol(x$R))
  
  
  ###########################
  #Cycle through the columns#
  ###########################
  for(col in 1:ncol(x$R)){
    
    
    ####################
    #Calculate the RMSD#
    ####################
    aves <- mat.or.vec(length(Spike.desc), 1)
    sds <- mat.or.vec(length(Spike.desc), 1)
    msd <- mat.or.vec(length(Spike.desc), table(x$genes[,wcol] == Spike.desc[1])["TRUE"])
    here <- mat.or.vec(length(Spike.desc), table(x$genes[,wcol] == Spike.desc[1])["TRUE"])
    use <- c()
    rep <- c()
    
    for(sp in 1:length(Spike.desc)){
      here[sp,] <- na.omit((1:length(x$genes[,wcol]))[x$genes[,wcol] == Spike.desc[sp]])
      use <- c(use, here[sp,])     
      rep <- c(rep, rep(Spike.expec[sp], table(x$genes[,wcol] == Spike.desc[sp])["TRUE"]))
      ratios <- log(x$R[here[sp,], col] / x$G[here[sp,], col], base)   
      aves[sp] <- median(ratios, na.rm=T)
      sds[sp] <- sd(ratios, na.rm=T)
      msd[sp,] <- (Spike.expec[sp] - ratios)**2
    }
    
    rmsd <- (mean(msd[is.finite(msd)], na.rm=T))**.5
    prermsd[col] <- rmsd
    
    M[, col] <- log(x$R[, col] / x$G[, col], base)
    A[, col] <- 0.5 * log(x$R[, col] * x$G[, col], base)
    
    
    ###############
    #Minimise RMSD#
    ###############
    lowestrmsd <- rmsd
    thissf <- 1
    
    for(sf in seq(0.01, 10, 0.1)){
      newM <- log((x$R[, col] * sf) / x$G[, col], base)
      newA <- 0.5 * log((x$R[, col] * sf)  * x$G[, col], base)
      rmsd <- 0
      count <- 0
      for(sp in 1:length(Spike.desc)){
        rmsd <- rmsd + sum((Spike.expec[sp] - newM[here[sp,]][is.finite( newM[here[sp,]])])**2)
        count <- count + ncol(here)
      }
      rmsd <- (rmsd/count)**0.5
      if(lowestrmsd > rmsd){
        lowestrmsd <- rmsd
        thissf <- sf
      }
    }
    
    
    for(sf in seq(thissf - 0.05, thissf + 0.05, 0.001)){
      newM <- log((x$R[, col] * sf) / x$G[, col], base)
      newA <- 0.5 * log((x$R[, col] * sf)  * x$G[, col], base)
      rmsd <- 0
      count <- 0
      for(sp in 1:length(Spike.desc)){
        rmsd <- rmsd + sum((Spike.expec[sp] - newM[here[sp,]][is.finite( newM[here[sp,]])])**2)
        count <- count + ncol(here)
      }
      rmsd <- (rmsd/count)**0.5
      if(lowestrmsd > rmsd){
        lowestrmsd <- rmsd
        thissf <- sf
      }
    }
    
    postrmsd[col] <- lowestrmsd
    
    M[, col] <- log((x$R[, col] * thissf) / x$G[ ,col], base)
    A[, col] <- 0.5 * log((x$R[, col] * thissf)  * x$G[ ,col], base)
    
    
    print(paste("Convergence to scaling factor=",thissf,sep=""))
    print(paste("Column ", col, " RMSD improved from ", prermsd[col], " to ", postrmsd[col], sep=""))
  }
  
  
  if(mva){
    INCA.MAPlot(x, wcol, spikeIn=T, SpikeFile, prefix="_pre-INCANormSI_", highlight=highlight)
  }
  
  newx <- x
  newx$R <- base ** (A + (M/2))
  newx$G <- base ** (A - (M/2))
  
  if(mva){
    INCA.MAPlot(newx, wcol, spikeIn=T, SpikeFile, prefix="_post-INCANormSI_", highlight=highlight)
  }
  
  return(newx)
}
