POS.Feature <-
function(positive_class, negative_class, test_seq){
if(class(positive_class)!="DNAStringSet"){stop("The positive_class must be of class DNAStringSet")}
if(length(unique(width(positive_class)))>1){stop("Each sequence of positive_class must of equal length")}

if(class(negative_class)!="DNAStringSet"){stop("The negative_class must be of class DNAStringSet")}
if(length(unique(width(negative_class)))>1){stop("Each sequence of negative_class must of equal length")}

if(class(test_seq)!="DNAStringSet"){stop("The test_seq must be of class DNAStringSet")}
if(length(unique(width(test_seq)))>1){stop("Each sequence of test_seq must be of equal length")}

zz <- as.character(as.character(test_seq))

tss <- as.matrix(positive_class)
fss <- as.matrix(negative_class)

tss[tss=="T"|tss=="TRUE"]<-"X"
fss[fss=="T"|fss=="TRUE"]<-"X"


#______________________________________________#
#Encoding with positional(POS) features using text delimited format sequence dataset#
#______________________________________________#

pwm <- function(s){ k1 <- sum((s=="A")*1)
                    k2 <- sum((s=="X")*1)
                    k3 <- sum((s=="G")*1)
                    k4 <- sum((s=="C")*1) 
                    k <-c(A=k1,X=k2,G=k3,C=k4)
                    k
                  }
pos_tss <- apply(tss,2,pwm)/nrow(tss)
pos_fss <- apply(fss,2,pwm)/nrow(fss)
pos_diff <- round(pos_fss - pos_tss, 3)



rep_pwm <- function(k){
  s <- unlist(strsplit(k, split=""))
  s[s=="T"|s=="TRUE"]<- "X"
  v <- vector(mode="numeric", length=4*length(s))
  v[which(s=="A")*4 - 3] <- pos_diff[1,which(s=="A")]
  v[which(s=="X")*4 - 2] <- pos_diff[2,which(s=="X")]
  v[which(s=="G")*4 - 1] <- pos_diff[3,which(s=="G")]
  v[which(s=="C")*4 - 0] <- pos_diff[4,which(s=="C")]
  v
   
}

enc_pos <- t(sapply(zz, rep_pwm))
enc_pos
}
