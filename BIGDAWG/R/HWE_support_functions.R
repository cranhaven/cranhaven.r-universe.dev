#' Genotype Combination Maker
#'
#' Make data frame of possible genotype combinations
#' @param x Number of alleles.
#' @note This function is for internal BIGDAWG use only.
makeComb <- function(x) {
  if(x >= 2) {
    tmp <- t(combn(x,2))
    tmp <- rbind(tmp,t(matrix(rep(1:x,2),byrow=T,ncol=x)))
    tmp <- tmp[do.call(order, as.data.frame(tmp)),]
    return(tmp)
  } else ( return(NA) )
}

#' Observed Frequency
#'
#' Get observed frequency of genotypes
#' @param x Single genotype.
#' @param genos.locus Locus genotypes.
#' @note This function is for internal BIGDAWG use only.
getObsFreq <- function(x,genos.locus) {
  
  if(x[1]==x[2]) {
    length(which(genos.locus[,1]==x[1] & genos.locus[,2]==x[2]))
  } else {
    return(sum(length(which(genos.locus[,1]==x[1] & genos.locus[,2]==x[2])),
               length(which(genos.locus[,1]==x[2] & genos.locus[,2]==x[1]))))
  }
}

#' Chi square matrices
#'
#' Chi Square contingency matrix builder with rare cell binning
#' @param Locus Locus of interest.
#' @param genos.sub Genotypes for locus of interest.
#' @param Allele.Freq Allele frequencies.
#' @param Allele.Combn Allele combinations.
#' @note This function is for internal BIGDAWG use only.
getCS.Mat <- function(Locus,genos.sub,Allele.Freq,Allele.Combn) {
  
  GTYPES <- Allele.Combn[[Locus]]
  
  if(!is.na(GTYPES)[1]) {
  
    nSID <- nrow(genos.sub)
    ColNames <- gsub(".1","",colnames(genos.sub),fixed=T) # column names
    genos.locus <- genos.sub[,which(ColNames==Locus)]
    freq <- Allele.Freq[[Locus]]
    GTYPES <- Allele.Combn[[Locus]]  
    GTYPES <- lapply(seq_len(nrow(GTYPES)), function(x) GTYPES[x,])
    
    #Expected Counts
    freq.Exp <- lapply(GTYPES,FUN=function(x) ifelse(x[1]==x[2],prod(freq[x])*nSID,2*prod(freq[x])*nSID))
    
    #Observed Counts
    GTYPES <- lapply(GTYPES,FUN=function(x) names(freq[x]))
    freq.Obs <- lapply(GTYPES,getObsFreq,genos.locus=genos.locus)
    
    freq.mat <- cbind(do.call(rbind,GTYPES),
                      do.call(rbind,freq.Obs),
                      do.call(rbind,freq.Exp))
    colnames(freq.mat) <- c("Allele.1","Allele.2","Obs","Exp")
    
    #bin rare cells
    freq.bin <- freq.mat[which(as.numeric(freq.mat[,'Exp'])<5),]
    freq.bin <- matrix(data=freq.bin,ncol=ncol(freq.mat),dimnames=dimnames(freq.mat))
    if(nrow(freq.bin)>0) {
      freq.bin <- matrix(c("binned.1","binned.2",sum(as.numeric(freq.bin[,'Obs'])),sum(as.numeric(freq.bin[,'Exp']))),
                         ncol=ncol(freq.bin),
                         dimnames=dimnames(freq.bin))
    }
    
    #Final Matrix for ChiSq
    if(nrow(freq.bin)>0) {
      freq.final <- rbind(freq.mat[which(as.numeric(freq.mat[,'Exp'])>=5),],freq.bin)
    } else {
      freq.final <- freq.mat
    }
    
    #Calculate (Obs - Exp)^2 / Exp
    if(nrow(freq.final)>1) {
      freq.final <- cbind(freq.final,
                          apply(freq.final[,c('Obs','Exp')],
                                MARGIN=1,
                                FUN=function(x) ((as.numeric(x['Obs']) - as.numeric(x['Exp']))^2)/as.numeric(x['Exp'])))
    } else {
      freq.final <- cbind(freq.final,0)
    }
    colnames(freq.final)[ncol(freq.final)] <- "O-E2|E"
    
    return(freq.final)
    
  } else { return(NA) }
  
}

#' Chi square test statistic
#'
#' Calculate chi square test statistic
#' @param Locus Locus of interest.
#' @param Freq.Final Contingency Matrix getCS.Mat output.
#' @note This function is for internal BIGDAWG use only.
getCS.stat <- function(Locus,Freq.Final) {
  
  df <- Freq.Final[[Locus]]
  if(!is.null(nrow(df))) {
    return(sum(as.numeric(df[,'O-E2|E'])))
  } else { return(NA) }
  
}

#' Recompute number of alleles
#'
#' Using Freq.Final, recompute number of alleles
#' @param x Locus specific contingency matrix getCS.Mat output.
#' @note This function is for internal BIGDAWG use only.
getAllele.Count <- function(x) {
  
  if(!is.null(nrow(x))) {
    return( length(unique(c(x[,'Allele.1'],x[,'Allele.2']))) )
  } else { return(NA) }
  
}

#' Hardy Weinbergy Equilibrium Function
#'
#' This is the workhorse function for each group analysis.
#' @param genos.sub data frame of genotype files post processing.
#' @param loci list of loci.
#' @param nloci number of loci in list
#' @note This function is for internal BIGDAWG use only.
HWE.ChiSq <- function(genos.sub,loci,nloci) {
  
  #Format genotypes
  df.1 <- data.frame(genos.sub[,seq(1,nloci*2,2)])
  df.2 <- data.frame(genos.sub[,seq(2,nloci*2,2)])
  colnames(df.2) <- colnames(df.1)
  df <- rbind(df.1,df.2)
  colnames(df) <- do.call(rbind,loci)
  rm(df.1,df.2)
  
  #Allele info
  Alleles <- lapply(loci,FUN=function(x) sort(unique(df[,x]))); names(Alleles) <- loci # unique allele names
  nAlleles <- lapply(loci,FUN=function(x) length(na.omit(unique(df[,x])))); names(nAlleles) <- loci # no. unique alleles
  nAlleles.tot <- lapply(loci,FUN=function(x) length(df[,x])); names(nAlleles.tot) <- loci # total no. alleles
  
  #Possible Genotypes
  Allele.Combn <- lapply(nAlleles,makeComb); names(Allele.Combn) <- loci
  
  #Get Allele Counts and Frequencies
  Allele.cnts <- lapply(loci,FUN=function(x) table(df[,x])); names(Allele.cnts) <- loci
  Allele.Freq <- lapply(loci,FUN=function(x) Allele.cnts[[x]]/nAlleles.tot[[x]]); names(Allele.Freq) <- loci
  
  #Get Observed and Expected Frequencies Matrix for genotypes
  Freq.Final <- lapply(loci,FUN=getCS.Mat,genos.sub=genos.sub,Allele.Freq=Allele.Freq,Allele.Combn=Allele.Combn)
  names(Freq.Final) <- loci
  
  #Calculate Chi Square Statistic for each Locus
  Freq.chisq <- lapply(loci,FUN=getCS.stat,Freq.Final=Freq.Final)
  names(Freq.chisq) <- loci
  
  #Recompute number of alleles and genotypes at each locus from binned contingency matrices (Freq.Final)
  nAlleles.bin <- lapply(Freq.Final,FUN=getAllele.Count)
  names(nAlleles.bin) <- loci
  nGenotypes.bin <- lapply(Freq.Final,nrow)
  names(nGenotypes.bin) <- loci
  nGenotypes.bin[which(as.numeric(lapply(nGenotypes.bin,FUN=is.null))==1)] <- NA
  
  #Get degrees of freedom for each locus
  #Alleles = a ; possible genotypes =g ; df = g - (a - 1)
  Allele.dof <- lapply(loci,FUN=function(x) nGenotypes.bin[[x]] - (nAlleles.bin[[x]] - 1) ); names(Allele.dof) <- loci
  
  #Get P.values from Chi Square distribution
  Freq.pvals <- lapply(loci,FUN=function(x) 1-pchisq(as.numeric(Freq.chisq[[x]]), as.numeric(Allele.dof[[x]])))
  names(Freq.pvals) <- loci
  
  #Format Output
  Test.out <- cbind(loci,
                    do.call(rbind,Freq.chisq),
                    do.call(rbind,Allele.dof),
                    do.call(rbind,Freq.pvals),
                    rep('NS',nloci))
  colnames(Test.out) <- c("Locus","X.square","df","p.value","sig")
  rownames(Test.out) <- NULL
  
  Test.out[which(as.numeric(Test.out[,'p.value'])<0.05),"sig"] <- "*"
  Test.out <- matrix(unlist(Test.out),ncol=ncol(Test.out),dimnames=dimnames(Test.out))
  rownames(Test.out) <- NULL
  
  Test.out[,'X.square'] <- sapply(as.numeric(Test.out[,'X.square']),FUN=round,digits=4)
  Test.out[,'p.value'] <- sapply(as.numeric(Test.out[,'p.value']),FUN=function(x) format.pval(x))
  
  #Flag for invalid degrees of freedom
  flagLoci <- which(as.numeric(Test.out[,'df'])<1)
  
  #Flag for invalid chi square matrices
  Freq.Flag <- lapply(loci,FUN=function(x) ifelse(nrow(Freq.Final[[x]])>2,0,1))
  flagLoci <- unique(c(flagLoci,which(Freq.Flag==1)))
  
  #Flag for invalid chi square matrices
  flagLoci <- unique(c(flagLoci,which(is.na(Test.out[,'X.square']))))
  
  if(length(flagLoci)>0){
    Test.out[Test.out[,'Locus'] %in% unlist(loci[flagLoci]),2:ncol(Test.out)] <- "NCalc"
  }  
  
  return(Test.out)
  
}
