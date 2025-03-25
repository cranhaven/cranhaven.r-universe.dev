#' INCATome Deregulated Genes Identification
#'
#' Performs the INCATome DEG identification for microarray data, consisting of an overlap of at least two out of four DEG tests (TTest, Limma, RankProd and SAM).
#' @param x an RGList object
#' @param cl a vector specifying type of samples, 0 being control and 1 being condition. 
#' @param wcol an integer specifying the number of the column where Gene Names can be found in the gene annotation table.
#' @param base an integer specifying the log base. Default is 2.
#' @param filt logical, TRUE if a set of negative control probes are to be used for filtering. Filtering is performed by removing any probes for which the average intensities are lower than the "negative" mean +/- 1.5 "negative" deviation. 
#' @param selneg a character or vector containing the negative control probe names for filtering.
#' @param highlight a character vector specifying a set of genes of interest. These will be highlighted in the graphical representations.
#' @return A List object containing the INCA DEG output for significant DEGs with INCA DEG Score >= 2, as well as all individual outputs from the different tests. Additionally, volcanoplots for each test will be generated.
#' @examples
#' #Load the INCATome Dataset
#' data(INCATomeData)
#' attach(INCATomeData)
#' out=INCA.DEG(RGdataDS,c(0,0,0,1,1,1),8,filt=TRUE,
#'  selneg="NegativeControl", highlight=c("ACTB","PABPC1"))
#' @import limma
#' @import multtest
#' @import siggenes
#' @import genefilter
#' @importFrom grDevices dev.off jpeg rainbow rgb
#' @importFrom graphics abline arrows legend points text
#' @importFrom stats cor median model.matrix na.omit sd
#' @importFrom utils write.table
#' @importFrom RankProd RP
#' @export

INCA.DEG <- function(x, cl, wcol, filt=TRUE, selneg, base = 2, highlight=NULL){
  
  if(class(x) != "RGList"){
    stop("Please supply an RGList")
  }
  if (missing(cl)){
    stop("The Comparison vector is missing")
  }
  if (filt==T && missing(selneg)){
    stop("The Negative Gene set is missing")
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
  
  
  MA=MA.RG(x)  
  usecl=(!is.na(cl))
  MA=MA[,usecl]
  newcl= cl[which(!is.na(cl))]
  
  if (filt==T){
    ###############
    #Filter probes#
    ###############
    negct=grep(paste("^", selneg,"$", sep=""),x$genes[,wcol])
    if(length(negct)==0){
      stop("Unable to find Negative Genes in the assigned GeneName column")
    }
    
    neg.means <- colMeans(MA$A[negct,], na.rm=T)
    neg.sds <- apply(MA$A[negct,], 2, sd, na.rm=T)
    filt.neg <- apply(MA$A[,], 1, function(x) length(grep("TRUE", x > neg.means + (1.5 * neg.sds))) >= (length(x)/2))
    keep <- filt.neg
    
    M <- MA$M[keep,]
  }else{
    keep=rep(T,length(MA$M[,1]))
    M <- MA$M
  }
  
  
  mean1 <- rowMeans(M[,which(newcl==1)], na.rm=T)
  mean2 <- rowMeans(M[,which(newcl==0)], na.rm=T)
  fc <- base ** (mean1 - mean2)
  
  
  ##########
  #RANKPROD#
  ##########
  
  RP <- RP(base**M, newcl, na.rm=FALSE, num.perm=100, logged=F)
  
  badjust <- mt.rawp2adjp(RP$pval[,1], proc=c("Bonferroni", "Hochberg", "BH"))$adjp
  aadjust <- mt.rawp2adjp(RP$pval[,2], proc=c("Bonferroni", "Hochberg", "BH"))$adjp
  badjust[order(RP$pval[,1]),] <- badjust
  aadjust[order(RP$pval[,2]),] <- aadjust
  
  #Print out the files
  rptable <- data.frame(MA$genes[keep,], 
                        
                        Mean1 = mean1, Mean2 = mean2, FC = fc, 
                        
                        Pfp = RP$pfp[,1], 
                        Pval = RP$pval[,1], 
                        RankProd = RP$RPs[,1], 
                        RankProdRank = RP$RPrank[,1], 
                        Bonferroni=badjust[,2], Hochberg.FWER=badjust[,3], BH.FDR=badjust[,4],
                        
                        Pfp = RP$pfp[,2], 
                        Pval = RP$pval[,2], 
                        RankProd = RP$RPs[,2], 
                        RankProdRank = RP$RPrank[,2], 
                        Bonferroni=aadjust[,2], Hochberg.FWER=aadjust[,3], BH.FDR=aadjust[,4],
                        
                        M[,which(newcl==0)],
                        M[,which(newcl==1)])
  
  names(rptable) <- c(colnames(MA$genes), "Mean CTRL", "Mean CDT", "FC", 
                      "CDT>CTRL pfp","CDT>CTRL pval","CDT>CTRL RP","CDT>CTRL RPRank","CDT>CTRL Bonferroni","CDT>CTRL Hochberg","CDT>CTRL FDR",
                      "CDT<CTRL pfp","CDT<CTRL pval","CDT<CTRL RP","CDT<CTRL RPRank","CDT<CTRL Bonferroni","CDT<CTRL Hochberg","CDT<CTRL FDR",
                      paste("CTRL",1:length(which(newcl==0)),sep=""),paste("CDT",1:length(which(newcl==1)),sep="")
  )
  
  write.table(rptable, file="INCA_DEG_RP_transEff.txt",
              row.names=F, qmethod="d", append=F, sep="\t")
  
  userp=which(rptable[,which(colnames(rptable)=="CDT>CTRL pval")] < 0.05 | rptable[,which(colnames(rptable)=="CDT<CTRL pval")] < 0.05)
  
  write.table(rptable[userp,], 
              file="INCA_DEG_RP_transEff_significant.txt",row.names=F, qmethod="d", append=F, sep="\t")
  
  
  minp <- 0
  maxp <- c(-log(RP$pval[,1], 10), -log(RP$pval[,2], 10))
  maxp <- max(maxp[is.finite(maxp)], na.rm=T)
  minfc <- log(fc,2)
  minfc <- min(minfc[is.finite(minfc)], na.rm=T)
  maxfc <- log(fc,2)
  maxfc <- max(maxfc[is.finite(maxfc)], na.rm=T)
  
  if (length(highlight)!=0){ 
    these <- c()
    for(m in 1:length(highlight)){
      these <- c(these, grep(paste("^", highlight[m], "$", sep=""), rptable[,wcol]))
    }
    hnames <- unique(rptable[these,wcol])
    hcols <- rainbow(length(hnames))
  }   
  
  jpeg("INCA_DEG_RP_volcano_transEff.jpg", quality=100, width=800, height=800)
  plot(log(fc,2), -log(RP$pval[,1], 10), 
       pch=19, col=rgb(0,0,0,0.3), 
       xlab="Log2(FC)", ylab="-Log10(Pval)", main="Volcano Plot, Rankprod", 
       xlim=c(minfc, maxfc), ylim=c(minp, maxp))
  
  if(length(hnames) > 0){
    for(m in 1:length(hnames)){
      this <- grep(paste("^", hnames[m], "$", sep=""), rptable[,wcol])
      points(log(fc[this],2), -log(RP$pval[this, 1], 10), col=hcols[m], pch=15)
      text(log(fc[this],2), -log(RP$pval[this, 1], 10), hnames[m], col=hcols[m], pos=4)
    }
  }
  points(log(fc,2), -log(RP$pval[,2], 10), pch=19, col=rgb(0,0,1,0.3))
  if(length(hnames) > 0){
    for(m in 1:length(hnames)){
      this <- grep(paste("^", hnames[m], "$", sep=""), rptable[,wcol])
      points(log(fc[this],2), -log(RP$pval[this, 2], 10), col=hcols[m], pch=15)
      text(log(fc[this],2), -log(RP$pval[this, 2], 10), hnames[m], col=hcols[m], pos=4)
    }
  }
  abline(v=0, col="black")
  abline(h=-log(0.05, 10), col="red")
  abline(v=log(0.5, 2), col="green")
  abline(v=log(2, 2), col="green")
  legend("topright", fill=c("black", "blue", "white", "white"), border=c("white", "white", "red", "green"), c("CDT>CTRL", "CDT<CTRL", "pval=0.05", "FC=+2/+0.5"))
  dev.off()
  
  
  ################################
  #SAM assessment of significance#
  ################################
  
  
  sam <- d.stat(M, newcl, var.equal=FALSE)
  
  samtable <- data.frame(MA$genes[keep,],
                         
                         Mean1 = mean1, Mean2 = mean2, FC = fc,
                         
                         ExpFalse = sam$vec.false, 
                         Pval = sam$p.value,  
                         
                         M[,which(newcl==0)],
                         M[,which(newcl==1)])
  
  names(samtable) <- c(colnames(MA$genes), "Mean CTRL", "Mean CDT", "FC", 
                       "SAM expFalse","SAM pval",
                       paste("CTRL",1:length(which(newcl==0)),sep=""),paste("CDT",1:length(which(newcl==1)),sep="")
  )
  
  write.table(samtable, file="INCA_DEG_SAM_transEff.txt",
              row.names=F, qmethod="d", append=F, sep="\t")
  
  usesam=which(samtable[,which(colnames(samtable)=="SAM pval")]<0.05)
  
  write.table(samtable[usesam,], 
              file="INCA_DEG_SAM_significant_transEff.txt",row.names=F, qmethod="d", append=F, sep="\t")
  

  
  jpeg("INCA_DEG_SAM_volcano_transEff.jpg", quality=100, width=800, height=800)
  plot(log(fc,2), -log(sam$p.value, 10), pch=19, col=rgb(0,0,0,0.3), xlab="Log2(FC)", ylab="-Log10(Pval)", main="Volcano Plot, SAM")
  if(length(hnames) > 0){
    for(m in 1:length(hnames)){
      this <- grep(paste("^", hnames[m], "$", sep=""), samtable[,wcol])
      points(log(fc[this],2), -log(sam$p.value[this], 10), col=hcols[m], pch=15)
      text(log(fc[this],2), -log(sam$p.value[this], 10), hnames[m], col=hcols[m], pos=4)
    }
  }
  abline(v=0, col="black")
  abline(h=-log(0.05, 10), col="red")
  abline(v=log(0.5, 2), col="green")
  abline(v=log(2, 2), col="green")
  legend("topright", fill=c("black", "white", "white"), border=c("white", "red", "green"), legend=c("data", "pval=0.05", "FC=+2/+0.5"))
  dev.off()
  
  ################################
  #############LIMMA##############
  ################################
  
  fit<-lmFit(M,model.matrix(~newcl))
  fit2<-eBayes(fit)
  tt<-topTable(fit2,number=nrow(fit2),coef=2,sort.by="none")
  
  limmatable <- data.frame(MA$genes[keep,],
                           
                           Mean1 = mean1, Mean2 = mean2, FC = fc,
                           
                           tstat = tt$t, 
                           Pval = tt$P.Value,  
                           adjPval = tt$adj.P.Val, 
                           Bonf = tt$B, 
                           
                           M[,which(newcl==0)],
                           M[,which(newcl==1)])
  
  names(limmatable) <- c(colnames(MA$genes), "Mean CTRL", "Mean CDT", "FC", 
                         "T Stat","pval","Adj pval","Bonf",
                         paste("CTRL",1:length(which(newcl==0)),sep=""),paste("CDT",1:length(which(newcl==1)),sep="")
  )
  
  write.table(limmatable, file="INCA_DEG_LIMMA_transEff.txt",
              row.names=F, qmethod="d", append=F, sep="\t")
  
  uselimma=which(limmatable[,which(colnames(limmatable)=="pval")]<0.05)
  
  write.table(limmatable[uselimma,], 
              file="INCA_DEG_LIMMA_significant_transEff.txt", row.names=F, qmethod="d", append=F, sep="\t")
  
  
  
  jpeg("INCA_DEG_LIMMA_volcano_transEff.jpg", quality=100, width=800, height=800)
  plot(log(fc,2), -log(tt$P.Value, 10), pch=19, col=rgb(0,0,0,0.3), xlab="Log2(FC)", ylab="-Log10(Pval)", main="Volcano Plot, Limma")
  if(length(hnames) > 0){
    for(m in 1:length(hnames)){
      this <- grep(paste("^", hnames[m], "$", sep=""), limmatable[,wcol])
      points(log(fc[this],2), -log(tt$P.Value[this], 10), col=hcols[m], pch=15)
      text(log(fc[this],2), -log(tt$P.Value[this], 10), hnames[m], col=hcols[m], pos=4)
    }
  }
  abline(v=0, col="black")
  abline(h=-log(0.05, 10), col="red")
  abline(v=log(0.5, 2), col="green")
  abline(v=log(2, 2), col="green")
  legend("topright", fill=c("black", "white", "white"), border=c("white", "red", "green"), legend=c("data", "pval=0.05", "FC=+2/+0.5"))
  dev.off()	
  
  ################################
  #############TTEST##############
  ################################
  ttest<-rowFtests(M,factor(newcl),var.equal=FALSE)
  
  ttesttable <- data.frame(MA$genes[keep,],
                           
                           Mean1 = mean1, Mean2 = mean2, FC = fc,
                           
                           Pval = ttest$p.value,   
                           
                           M[,which(newcl==0)],
                           M[,which(newcl==1)])
  
  names(ttesttable) <- c(colnames(MA$genes), "Mean CTRL", "Mean CDT", "FC", 
                         "pval",
                         paste("CTRL",1:length(which(newcl==0)),sep=""),paste("CDT",1:length(which(newcl==1)),sep="")
  )
  
  write.table(ttesttable, file="INCA_DEG_TTEST_transEff.txt",
              row.names=F, qmethod="d", append=F, sep="\t")
  
  usettest=which(ttesttable[,which(colnames(ttesttable)=="pval")]<0.05)
  
  write.table(ttesttable[usettest,], 
              file="INCA_DEG_TTEST_significant_transEff.txt",row.names=F, qmethod="d", append=F, sep="\t")
  
  
  
  jpeg("INCA_DEG_TTEST_volcano_transEff.jpg", quality=100, width=800, height=800)
  plot(log(fc,2), -log(ttest$p.value, 10), pch=19, col=rgb(0,0,0,0.3), xlab="Log2(FC)", ylab="-Log10(Pval)", main="Volcano Plot, ttest")
  if(length(hnames) > 0){
    for(m in 1:length(hnames)){
      this <- grep(paste("^", hnames[m], "$", sep=""), ttesttable[,wcol])
      points(log(fc[this],2), -log(ttest$p.value[this], 10), col=hcols[m], pch=15)
      text(log(fc[this],2), -log(ttest$p.value[this], 10), hnames[m], col=hcols[m], pos=4)
    }
  }
  abline(v=0, col="black")
  abline(h=-log(0.05, 10), col="red")
  abline(v=log(0.5, 2), col="green")
  abline(v=log(2, 2), col="green")
  legend("topright", fill=c("black", "white", "white"), border=c("white", "red", "green"), legend=c("data", "pval=0.05", "FC=+2/+0.5"))
  dev.off()
  
  
  
  
  rppval=c()
  for(i in 1:length(which(keep==T))){
    if (is.na(RP$pval[i,1])){
      rppval=c(rppval,RP$pval[i,1])
    }else if (RP$pval[i,1]<=0.05) {
      rppval=c(rppval,RP$pval[i,1])
    } else if (RP$pval[i,2]<=0.05) {
      rppval=c(rppval,RP$pval[i,2])
    } else {
      rppval=c(rppval,mean(c(RP$pval[i,1],RP$pval[i,2])))
    }
  }
  
  combinedtable <- data.frame(MA$genes[keep,],
                              
                              Mean1 = mean1, Mean2 = mean2, FC = fc,
                              
                              Score= rep(NA,length(which(keep==T))),
                              PvalTT = ttest$p.value,
                              PvalLI = tt$P.Value,
                              PvalRP = rppval,
                              PvalSA = sam$p.value,
                              
                              M[,which(newcl==0)],
                              M[,which(newcl==1)])
  
  
  
  for(j in 1:length(which(keep==T))){
    combinedtable$Score[j]=length(which(c( (combinedtable$PvalTT[j]<=0.05),(combinedtable$PvalLI[j]<=0.05),if(!is.na(combinedtable$PvalRP[j])){(combinedtable$PvalRP[j]<=0.05)},(combinedtable$PvalSA[j]<=0.05) ) ==T))
  }  					
  
  
  names(combinedtable) <- c(colnames(MA$genes), "Mean CTRL", "Mean CDT", "FC", 
                            "Score","Pval TTest","Pval Limma","Pval RP","Pval SAM",
                            paste("CTRL",1:length(which(newcl==0)),sep=""),paste("CDT",1:length(which(newcl==1)),sep="")
  )
  
  uselite=which(combinedtable$Score>=2&(combinedtable$FC>=2 | combinedtable$FC<=0.5))
  
  combinedtablelite=combinedtable[uselite,]  
  
  write.table(combinedtablelite, file="INCA_DEG combined table.txt",
              row.names=F, qmethod="d", append=F, sep="\t")
  
return(list(INCADEG=combinedtablelite,Ttest=ttesttable,Limma=limmatable,RP=rptable,SAM=samtable))
}