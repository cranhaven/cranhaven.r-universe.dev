#' INCATome Normalisation by Internal Control Probes
#'
#' Performs the INCATome normalisation using invariance of Internal Control probes selected by the user for microarray data.
#' @param x an RGList object
#' @param InternalFile a data.frame specifying the names of the array files (in a column called "FileName") and the expected log2 ratios for two internal control genes selected by the user (respectively in columns headed with the gene names). Expected log2 ratios are to be acquired experimentally, for each corresponding sample (typically by northern blotting or qPCR).
#' @param wcol an integer specifying the number of the column where Gene Names can be found in the gene annotation table.
#' @param base an integer specifying the log base. Default is 2.
#' @param mva logical, TRUE to plot MA plots before and after normalisation for each array.
#' @return A new RGList object containing the normalised array data. Additionally, if mva is TRUE, MA plots before and after normalisations will be generated for each arrays.
#' @examples
#' #Load the INCATome Dataset
#' data(INCATomeData)
#' attach(INCATomeData)
#' dc=INCA.NormIC(RGdataBG,idata,8)
#' @importFrom grDevices dev.off jpeg rainbow rgb
#' @importFrom graphics abline arrows legend points text
#' @importFrom stats cor median model.matrix na.omit sd
#' @importFrom utils write.table
#' @export

INCA.NormIC <- function(x, InternalFile, wcol, base = 2, mva=TRUE){
  
  
  if(class(x) != "RGList"){
    stop("Please supply an RGList with agilent data as the first argument")
  }
  if (missing(InternalFile)){
    stop("The Internal Control File is missing")
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
  
  
  prermsd <- mat.or.vec(ncol(x$R), 1)
  postrmsd <- mat.or.vec(ncol(x$R), 1)
  
  M <- mat.or.vec(nrow(x$R), ncol(x$R))
  A <- mat.or.vec(nrow(x$R), ncol(x$R))
  
  Indpt.desc <- colnames(InternalFile)[2:3]
  if(length(Indpt.desc)==0){
    stop("Unable to read Internal control genenames (table header in InternalFile)")
  }
  
  
  ###########################
  #Cycle through the columns#
  ###########################
  for(col in 1:ncol(x$R)){
    
    here<-c()
    here1=c()
    here2=c()
    Indpt.expec<-c()
    Fract.expec<-c()
    aves<-c()
    sds<-c()
    msd<-c()
    Fract<-c()
    ratios<-c()
    R<-c()
    rmsd<-c()
    lowestrmsd<-c()
    
    Indpt.expec<-c(InternalFile[col,2],InternalFile[col,3])
    Fract.expec<-c(Indpt.expec[1]/(abs(Indpt.expec[1])+abs(Indpt.expec[2])),Indpt.expec[2]/(abs(Indpt.expec[1])+abs(Indpt.expec[2])))
    
    here1=grep(paste("^", Indpt.desc[1],"$", sep=""),x$genes[,wcol])
    here2=grep(paste("^", Indpt.desc[2],"$", sep=""),x$genes[,wcol])
    if(length(here1)==0){
      stop(paste("Unable to find Internal control ",Indpt.desc[1], " in gene list",sep=""))
    }
    if(length(here2)==0){
      stop(paste("Unable to find Internal control ",Indpt.desc[2], " in gene list",sep=""))
    }
    
    d=max(length(here1),length(here2))
    here<-matrix(NA,ncol=2,nrow=d)
    here[1:length(here1),1]=here1
    here[1:length(here2),2]=here2
    
    aves<- mat.or.vec(length(Indpt.desc), 1)
    sds<- mat.or.vec(length(Indpt.desc), 1)
    msd <- mat.or.vec(2, d)
    use <- c()
    rep <- c()
    
    #Fract<-mat.or.vec(2, 13)
    ratios<-mat.or.vec(2, d)
    
    for(sp in 1:2){
      ratios[sp,] <- log(x$R[here[,sp], col] / x$G[here[,sp], col], base)	
    }
    
    R<-c(mean(na.omit(ratios[1,])),mean(na.omit(ratios[2,])))
    
    if( (Indpt.expec[1]>Indpt.expec[2] & R[1]<R[2]) | (Indpt.expec[1]<Indpt.expec[2] & R[1]>R[2])){
      print("Warning: the invariant spots do not follow predicted pattern. Normalisation has been approximated.")
      if (Fract.expec[1]/Fract.expec[2]<0){
        i<-grep("-",Fract.expec)
        Fract.expec[i] <- 0.50
        Fract.expec[c(1,2)[-i]] <- -0.50
      }else{
        stop("Unable to perform INCA.NormIC: error1 see R code")
      }
    }
    
    Fract[1]<-R[1]/(abs(R[1])+abs(R[2]))
    Fract[2]<-R[2]/(abs(R[1])+abs(R[2]))
    
    aves <- median(Fract, na.rm=T)
    sds <- sd(Fract, na.rm=T)
    msd <- (Fract.expec - Fract)**2
    
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
      
      for(sp in 1:2){
        ratios[sp,] <- newM[here[,sp]]
      }
      
      R<-c(mean(na.omit(ratios[1,])),mean(na.omit(ratios[2,])))
      
      if( (Indpt.expec[1]>Indpt.expec[2] & R[1]<R[2]) | (Indpt.expec[1]<Indpt.expec[2] & R[1]>R[2])){
        if (Fract.expec[1]/Fract.expec[2]<0){
          i<-grep("-",Fract.expec)
          Fract.expec[i] <- 0.50
          Fract.expec[c(1,2)[-i]] <- -0.50
        }else{
          stop("Unable to perform INCA.NormIC: error2 see R code")
        }
      }
      
      Fract[1]<-R[1]/(abs(R[1])+abs(R[2]))
      Fract[2]<-R[2]/(abs(R[1])+abs(R[2]))
      
      aves <- median(Fract, na.rm=T)
      sds <- sd(Fract, na.rm=T)
      msd <- (Fract.expec - Fract)**2     
      rmsd <- (mean(msd[is.finite(msd)], na.rm=T))**.5
      
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
      
      for(sp in 1:2){
        ratios[sp,] <- newM[here[,sp]]
      }
      
      R<-c(mean(na.omit(ratios[1,])),mean(na.omit(ratios[2,])))
      
      if( (Indpt.expec[1]>Indpt.expec[2] & R[1]<R[2]) | (Indpt.expec[1]<Indpt.expec[2] & R[1]>R[2])){
        if (Fract.expec[1]/Fract.expec[2]<0){
          i<-grep("-",Fract.expec)
          Fract.expec[i] <- 0.50
          Fract.expec[c(1,2)[-i]] <- -0.50
        }else{
          stop("Unable to perform INCA.NormIC: error3 see R code")
        }
      }
      
      Fract[1]<-R[1]/(abs(R[1])+abs(R[2]))
      Fract[2]<-R[2]/(abs(R[1])+abs(R[2]))
      
      aves <- median(Fract, na.rm=T)
      sds <- sd(Fract, na.rm=T)
      msd <- (Fract.expec - Fract)**2    
      rmsd <- (mean(msd[is.finite(msd)], na.rm=T))**.5
      
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
    INCA.MAPlot(x, wcol, spikeIn=F, prefix="_pre-INCANormIC_", highlight=Indpt.desc)
  }
  
  newx <- x
  newx$R <- base ** (A + (M/2))
  newx$G <- base ** (A - (M/2))
  
  if(mva){
    INCA.MAPlot(newx, wcol, spikeIn=F, prefix="_post-INCANormIC_", highlight=Indpt.desc)
  }
  
  return(newx)
}