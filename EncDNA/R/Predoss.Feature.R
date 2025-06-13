Predoss.Feature <-
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


#_____________________________________________________________#
#finding the conditional expectation for the true splice site#
#____________________________________________________________#
cond_exp <- function(ss,pos1,pos2)
{
  seqlen <- ncol(ss)
  ce <- matrix(0,nrow=seqlen,ncol=seqlen)
  for(i in 1:seqlen)
  {
    for(j in (1:seqlen)[-i])
    {
        p12 <- sum(ss[,i]==pos1 & ss[,j]==pos2)
        p1 <- sum(ss[,i]==pos1)
        p2 <- sum(ss[,j]==pos2)
        ce[i,j] <- p12/(sqrt(p1*p2)+0.000001)
    }
  }
  ce
 }
#__________________________________________________#
#saving the association file for True splice site#
#__________________________________________________#
 AAt <- as.numeric(cond_exp(xt,"A","A"))
 TTt <- as.numeric(cond_exp(xt,"X","X"))
 GGt <- as.numeric(cond_exp(xt,"G","G"))
 CCt <- as.numeric(cond_exp(xt,"C","C"))
 ACt <- as.numeric(cond_exp(xt,"A","C"))
 ATt <- as.numeric(cond_exp(xt,"A","X"))
 AGt <- as.numeric(cond_exp(xt,"A","G"))
 TGt <- as.numeric(cond_exp(xt,"X","G"))
 TCt <- as.numeric(cond_exp(xt,"X","C"))
 TAt <- as.numeric(cond_exp(xt,"X","A"))
 GAt <- as.numeric(cond_exp(xt,"G","A"))
 GTt <- as.numeric(cond_exp(xt,"G","X"))
 GCt <- as.numeric(cond_exp(xt,"G","C"))
 CTt <- as.numeric(cond_exp(xt,"C","X"))
 CGt <- as.numeric(cond_exp(xt,"C","G"))
 CAt <- as.numeric(cond_exp(xt,"C","A"))
 cbt <- cbind(AAt, ATt, AGt, ACt, TAt, TTt, TGt, TCt, GAt, GTt, GGt, GCt, CAt, CTt, CGt, CCt)
 
#_________________________________________________________#
#saving the association file for False splice site#
#_________________________________________________________#
 AAf <- as.numeric(cond_exp(xf,"A","A"))
 TTf <- as.numeric(cond_exp(xf,"X","X"))
 GGf <- as.numeric(cond_exp(xf,"G","G"))
 CCf <- as.numeric(cond_exp(xf,"C","C"))
 ACf <- as.numeric(cond_exp(xf,"A","C"))
 ATf <- as.numeric(cond_exp(xf,"A","X"))
 AGf <- as.numeric(cond_exp(xf,"A","G"))
 TGf <- as.numeric(cond_exp(xf,"X","G"))
 TCf <- as.numeric(cond_exp(xf,"X","C"))
 TAf <- as.numeric(cond_exp(xf,"X","A"))
 GAf <- as.numeric(cond_exp(xf,"G","A"))
 GTf <- as.numeric(cond_exp(xf,"G","X"))
 GCf <- as.numeric(cond_exp(xf,"G","C"))
 CTf <- as.numeric(cond_exp(xf,"C","X"))
 CGf <- as.numeric(cond_exp(xf,"C","G"))
 CAf <- as.numeric(cond_exp(xf,"C","A"))
 cbf <- cbind(AAf, ATf, AGf, ACf, TAf, TTf, TGf, TCf, GAf, GTf, GGf, GCf, CAf, CTf, CGf, CCf)
 
#_________________________________________________________#

diff <- round(cbt-cbf,3)
seqlen <- ncol(xt)
arr <- array(diff, dim=c(nrow=seqlen, ncol=seqlen, size=16))

#________________________________________________________#
#encoding of  sequence data #
#_________________________________________________________# 
seq_encode <- function(p){
s <- unlist(strsplit(p, split=""))
s[s=="T"|s=="TRUE"]<- "X"
seqlen <- length(s)
z <- matrix(0, nrow=seqlen, ncol=seqlen)
for(j in 1:seqlen){
    for(k in (1:seqlen)[-j]){
        if(s[j]=="A" && s[k]=="A"){z[j, k]<- arr[j, k, 1] }
        if(s[j]=="A" && s[k]=="T"){z[j, k]<- arr[j, k, 2] }
        if(s[j]=="A" && s[k]=="G"){z[j, k]<- arr[j, k, 3] }
        if(s[j]=="A" && s[k]=="C"){z[j, k]<- arr[j, k, 4] }
      
        if(s[j]=="T" && s[k]=="A"){z[j, k]<- arr[j, k, 5] }
        if(s[j]=="T" && s[k]=="T"){z[j, k]<- arr[j, k, 6] }
        if(s[j]=="T" && s[k]=="G"){z[j, k]<- arr[j, k, 7] }
        if(s[j]=="T" && s[k]=="C"){z[j, k]<- arr[j, k, 8] }
      
        if(s[j]=="G" && s[k]=="A"){z[j, k]<- arr[j, k, 9] }
        if(s[j]=="G" && s[k]=="T"){z[j, k]<- arr[j, k, 10] }
        if(s[j]=="G" && s[k]=="G"){z[j, k]<- arr[j, k, 11] }
        if(s[j]=="G" && s[k]=="C"){z[j, k]<- arr[j, k, 12] }
      
        if(s[j]=="C" && s[k]=="A"){z[j, k]<- arr[j, k, 13] }
        if(s[j]=="C" && s[k]=="T"){z[j, k]<- arr[j, k, 14] }
        if(s[j]=="C" && s[k]=="G"){z[j, k]<- arr[j, k, 15] }
        if(s[j]=="C" && s[k]=="C"){z[j, k]<- arr[j, k, 16] }
}
}
z
az <- as.numeric(z)
az
}

predoss_enc <- t(sapply(zz, seq_encode))
predoss_enc 
}
