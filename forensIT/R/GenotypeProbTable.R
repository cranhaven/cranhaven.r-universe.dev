#' @title Genotype Probability Table
#' @description Genotype Probability Table
#' @param bbn1 Bayesian network
#' @param resQQ results from bn
#' @param bplot boolean to plot
#' @param numMarkers number of markers
#' @param lLoci list of loci
#' @return Genotype Probability Table
#' @export
genotypeProbTable <- function(bbn1,resQQ,bplot=FALSE,numMarkers=4,lLoci){
  
  
  sysName<-unlist(lapply(strsplit(names(resQQ),"_",fixed=TRUE),function(x){return(x[2])}))
  lprobPconj <- lprobMconj <- lprobP <- lprobM <- lprobG<-list()
  for(i in seq_along(bbn1$alelFreq)){
    
    iop<-match(resQQ[[i]][,1],lLoci[[sysName[i]]][,c("Alelo")])
    iom<-match(resQQ[[i+(length(resQQ)/2)]][,1],lLoci[[sysName[i]]][,c("Alelo")])
    pPopFreqP <- lLoci[[sysName[i]]][iop,]             # prob poblacional
    pBNetP    <- pBNetP1   <- resQQ[[i]];                  # pbNet1P1:prob conjunta
    pBNetP[,"prob"]<-pBNetP[,"prob"]/sum(pBNetP[,"prob"])  # prob condicional
    
    pPopFreqM <- lLoci[[sysName[i]]][iom,]              # prob poblacional
    pBNetM    <- pBNetM1   <- resQQ[[i+(length(resQQ)/2)]]; # pbNet1M1:prob conjunta
    pBNetM[,"prob"]<-pBNetM[,"prob"]/sum(pBNetM[,"prob"])   # prob condicional
    pBNp<-cbind(bnetP=pBNetP[,"prob"],pop=pPopFreqP[,"freq"])
    pBNm<-cbind(bnetM=pBNetM[,"prob"],pop=pPopFreqM[,"freq"])
    rownames(pBNp)<-pPopFreqP[,"Alelo"]
    rownames(pBNm)<-pPopFreqM[,"Alelo"]
    pBNp1<-cbind(bnetP=pBNetP1[,"prob"],pop=pPopFreqP[,"freq"])
    pBNm1<-cbind(bnetM=pBNetM1[,"prob"],pop=pPopFreqM[,"freq"])
    rownames(pBNp1)<-pPopFreqP[,"Alelo"]
    rownames(pBNm1)<-pPopFreqM[,"Alelo"]
    lprobP[[sysName[i]]] <- pBNp
    lprobM[[sysName[i]]] <- pBNm
    
    lprobPconj[[sysName[i]]] <- pBNp1
    lprobMconj[[sysName[i]]] <- pBNm1
    
    pp <- pBNp[,1];names(pp) <- rownames(pBNp)
    mm <- pBNm[,1];names(mm) <- rownames(pBNm)
    aux    <- genotypeProbs(pp,mm)
    
    pp <- pBNp1[,1];names(pp) <- rownames(pBNp1)
    mm <- pBNm1[,1];names(mm) <- rownames(pBNm1)
    auxConj <- genotypeProbs(pp,mm)
    aux2    <- genotypeProbs(bbn1$alelFreq[[sysName[i]]],bbn1$alelFreq[[sysName[i]]])
    vbnet <- vconj <- aux2 * 0 
    vbnet[names(aux)]     <- aux
    vconj[names(auxConj)] <- auxConj
    lprobG[[sysName[i]]]<-data.frame(geno=names(aux2),pop=aux2,bnet=vbnet,geno_conj=vconj)
    if(bplot && i<=numMarkers){    
      barplot(t(lprobP[[sysName[i]]]),beside=TRUE,col=c("pink","gray"),main=paste("P:",sysName[i]))
      barplot(t(lprobM[[sysName[i]]]),beside=TRUE,col=c("pink","gray"),main=paste("M:",sysName[i]))
      plot(aux,aux2,log10="xy",xlab="p_geno bnet",ylab="p_geno pop")
      abline(0,1,col="grey",lty=2)
      
      legend("topleft",c(paste("KL(p_bnet,p_pop):",signif(KLd(aux,aux2),2)),
                         paste("KL(p_pop,p_bnet):",signif(KLd(aux2,aux),2))),cex=0.8,bty="n")
    }
  }
  return(list(lprobG=lprobG,lprobP=lprobP,lprobM=lprobM))
}
