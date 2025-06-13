SAE.Feature <-
function(positive_class, negative_class, test_seq){
if(class(positive_class)!="DNAStringSet"){stop("The positive_class must be of class DNAStringSet")}
if(length(unique(width(positive_class)))>1){stop("Each sequence of positive_class must of equal length")}

if(class(negative_class)!="DNAStringSet"){stop("The negative_class must be of class DNAStringSet")}
if(length(unique(width(negative_class)))>1){stop("Each sequence of negative_class must of equal length")}

if(class(test_seq)!="DNAStringSet"){stop("The test_seq must be of class DNAStringSet")}
if(length(unique(width(test_seq)))>1){stop("Each sequence of test_seq must be of equal length")}
zz <- as.character(as.character(test_seq))

xt <- as.matrix(positive_class)
xf <- as.matrix(negative_class)
xt[xt=="T"|xt=="TRUE"]<-"X"
xf[xf=="T"|xf=="TRUE"]<-"X"

tr.t.ds <- xt
tr.f.ds <- xf
seqlen <- ncol(xt)


#_______________________________________________________________________#
#finding estimated value#
#_______________________________________________________________________#
    ind_est<- function(seqs,pos1,pos2)
   {
      est_seq <- matrix(0,nrow=seqlen,ncol=seqlen)
       for(i in 1:seqlen)
      {
        for(j in (1:seqlen)[-i])
         {
           p12 <- sum(seqs[,i]==pos1 & seqs[,j]==pos2)
           p2 <- sum(seqs[,j]==pos2)
           est_seq[i,j] <- p12/(p2+0.000001)
        }
     }
      est_seq
   }
#_____________________________________________________________#
#Estimated value obtained from the training dataset of true splice sites#
#__________________________________________________________________________________#
       tAA <- ind_est(tr.t.ds,"A","A")
       tAT <- ind_est(tr.t.ds,"A","X")
       tAG <- ind_est(tr.t.ds,"A","G")
       tAC <- ind_est(tr.t.ds,"A","C")
       tTA <- ind_est(tr.t.ds,"X","A")
       tTT <- ind_est(tr.t.ds,"X","X")
       tTG <- ind_est(tr.t.ds,"X","G")
       tTC <- ind_est(tr.t.ds,"X","C")
       tGA <- ind_est(tr.t.ds,"G","A")
       tGT <- ind_est(tr.t.ds,"G","X")
       tGC <- ind_est(tr.t.ds,"G","C")
       tGG <- ind_est(tr.t.ds,"G","G")
       tCT <- ind_est(tr.t.ds,"C","X")
       tCG <- ind_est(tr.t.ds,"C","G")
       tCA <- ind_est(tr.t.ds,"C","A")
       tCC <- ind_est(tr.t.ds,"C","C")
       cbt <- cbind(tAA, tAT, tAG, tAC, tTA, tTT, tTG, tTC, tGA, tGT, tGG, tGC, tCA, tCT, tCG, tCC)
       arrt <- array(cbt, dim=c(nrow=seqlen, ncol=seqlen, size=16))
       
#__________________________________________________________________________________#
#saving the estimated value obtained from the training dataset of false splice sites#
#__________________________________________________________________________________#
       fAA <- ind_est(tr.f.ds,"A","A")
       fAT <- ind_est(tr.f.ds,"A","X")
       fAG <- ind_est(tr.f.ds,"A","G")
       fAC <- ind_est(tr.f.ds,"A","C")
       fTA <- ind_est(tr.f.ds,"X","A")
       fTT <- ind_est(tr.f.ds,"X","X")
       fTG <- ind_est(tr.f.ds,"X","G")
       fTC <- ind_est(tr.f.ds,"X","C")
       fGA <- ind_est(tr.f.ds,"G","A")
       fGT <- ind_est(tr.f.ds,"G","X")
       fGC <- ind_est(tr.f.ds,"G","C")
       fGG <- ind_est(tr.f.ds,"G","G")
       fCT <- ind_est(tr.f.ds,"C","X")
       fCG <- ind_est(tr.f.ds,"C","G")
       fCA <- ind_est(tr.f.ds,"C","A")
       fCC <- ind_est(tr.f.ds,"C","C")
       cbf <- cbind(fAA, fAT, fAG, fAC, fTA, fTT, fTG, fTC, fGA, fGT, fGG, fGC, fCA, fCT, fCG, fCC)
       arrf <- array(cbf, dim=c(nrow=seqlen, ncol=seqlen, size=16))
       
#______________________________________________________________________________________#
#finding sum of absolute value of error for the test sequence through true splice sites#
#_____________________________________________________________________________________#
 
absol_err_t<- function(s){
test <- unlist(strsplit(s, split=""))
test[test=="T"|test=="TRUE"]<- "X"
E <- matrix(0, nrow=seqlen, ncol=seqlen)

  for(j in 1:seqlen)
    {
    for(k in (1:seqlen)[-j])
      {
        if(test[j]=="A" && test[k]=="A"){E[j,k]<-arrt[j,k,1]}
        if(test[j]=="A" && test[k]=="X"){E[j,k]<-arrt[j,k,2]}
        if(test[j]=="A" && test[k]=="G"){E[j,k]<-arrt[j,k,3]}
        if(test[j]=="A" && test[k]=="C"){E[j,k]<-arrt[j,k,4]}
      
        if(test[j]=="X" && test[k]=="A"){E[j,k]<-arrt[j,k,5]}
        if(test[j]=="X" && test[k]=="X"){E[j,k]<-arrt[j,k,6]}
        if(test[j]=="X" && test[k]=="G"){E[j,k]<-arrt[j,k,7]}
        if(test[j]=="X" && test[k]=="C"){E[j,k]<-arrt[j,k,8]}
      
        if(test[j]=="G" && test[k]=="A"){E[j,k]<-arrt[j,k,9]}
        if(test[j]=="G" && test[k]=="X"){E[j,k]<-arrt[j,k,10]}
        if(test[j]=="G" && test[k]=="G"){E[j,k]<-arrt[j,k,11]}
        if(test[j]=="G" && test[k]=="C"){E[j,k]<-arrt[j,k,12]}
      
        if(test[j]=="C" && test[k]=="A"){E[j,k]<-arrt[j,k,13]}
        if(test[j]=="C" && test[k]=="X"){E[j,k]<-arrt[j,k,14]}
        if(test[j]=="C" && test[k]=="G"){E[j,k]<-arrt[j,k,15]}
        if(test[j]=="C" && test[k]=="C"){E[j,k]<-arrt[j,k,16]}
      
      }
    }
     E
     zt <- 1-E
     d <- abs(zt)
     ABEt <- sum(d)
     ABEt
 }
 
 #______________________________________________________________________________________#
#finding sum of absolute value of error for the test sequence through false splice sites#
#_____________________________________________________________________________________#

absol_err_f <- function(s)
 {
test <- unlist(strsplit(s, split=""))
test[test=="T"|test=="TRUE"]<- "X"
E <- matrix(0, nrow=seqlen, ncol=seqlen)
       for(j in 1:seqlen)
    {
      for(k in (1:seqlen)[-j])
       {
        if(test[j]=="A" && test[k]=="A"){E[j,k]<-arrf[j,k,1]}
        if(test[j]=="A" && test[k]=="T"){E[j,k]<-arrf[j,k,2]}
        if(test[j]=="A" && test[k]=="G"){E[j,k]<-arrf[j,k,3]}
        if(test[j]=="A" && test[k]=="C"){E[j,k]<-arrf[j,k,4]}
      
        if(test[j]=="T" && test[k]=="A"){E[j,k]<-arrf[j,k,5]}
        if(test[j]=="T" && test[k]=="T"){E[j,k]<-arrf[j,k,6]}
        if(test[j]=="T" && test[k]=="G"){E[j,k]<-arrf[j,k,7]}
        if(test[j]=="T" && test[k]=="C"){E[j,k]<-arrf[j,k,8]}
      
        if(test[j]=="G" && test[k]=="A"){E[j,k]<-arrf[j,k,9]}
        if(test[j]=="G" && test[k]=="T"){E[j,k]<-arrf[j,k,10]}
        if(test[j]=="G" && test[k]=="G"){E[j,k]<-arrf[j,k,11]}
        if(test[j]=="G" && test[k]=="C"){E[j,k]<-arrf[j,k,12]}
      
        if(test[j]=="C" && test[k]=="A"){E[j,k]<-arrf[j,k,13]}
        if(test[j]=="C" && test[k]=="T"){E[j,k]<-arrf[j,k,14]}
        if(test[j]=="C" && test[k]=="G"){E[j,k]<-arrf[j,k,15]}
        if(test[j]=="C" && test[k]=="C"){E[j,k]<-arrf[j,k,16]}
      
      }
    }
     E
     zf <- 1-E
     d <- abs(zf)
     ABEf <- sum(d)
     ABEf
 }
#________________________________________________________________________#
#finding sum of absolute error of test instance by assuming it is as FSS#
#_______________________________________________________________________#
#________________________________________________________________________#
#finding sum of absolute error of test instance by assuming it is as TSS#
#_______________________________________________________________________#
SAE_t <- sapply(zz, absol_err_t)
SAE_f <- sapply(zz, absol_err_f)
#________________________________________________#
#finding difference in the sum of absolute error#
#_______________________________________________#
DSAE <- SAE_f - SAE_t 
  #____________________________________________________________________________#
cbind(SAE_t, DSAE)
}
