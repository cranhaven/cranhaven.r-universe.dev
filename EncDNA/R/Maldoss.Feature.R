Maldoss.Feature <-
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

#__________________________________________#
#Compute the di-nucleotide frequency matrix using tr.tss#
#_______________________________________________________#

din.freq <- function (test){
	seqlen <- ncol(test)
	numseq <- nrow(test)
	zt <- matrix(0, nrow=16, ncol=seqlen-1)
		for(i in 1:(seqlen-1)){
			for(j in (i+1)){
			zt[1,i] <- log(sum(test[,i]=="A" & test[,j]=="A")+1, base=2)
			zt[2,i] <- log(sum(test[,i]=="A" & test[,j]=="X")+1, base=2)
			zt[3,i] <- log(sum(test[,i]=="A" & test[,j]=="G")+1, base=2)
			zt[4,i] <- log(sum(test[,i]=="A" & test[,j]=="C")+1, base=2)
			
			zt[5,i] <- log(sum(test[,i]=="X" & test[,j]=="A")+1, base=2)
			zt[6,i] <- log(sum(test[,i]=="X" & test[,j]=="X")+1, base=2)
			zt[7,i] <- log(sum(test[,i]=="X" & test[,j]=="G")+1, base=2)
			zt[8,i] <- log(sum(test[,i]=="X" & test[,j]=="C")+1, base=2)
			
			zt[9,i] <- log(sum(test[,i]=="G" & test[,j]=="A")+1, base=2)
			zt[10,i] <- log(sum(test[,i]=="G" & test[,j]=="X")+1, base=2)
			zt[11,i] <- log(sum(test[,i]=="G" & test[,j]=="G")+1, base=2)
			zt[12,i] <- log(sum(test[,i]=="G" & test[,j]=="C")+1, base=2)
			
			zt[13,i] <- log(sum(test[,i]=="C" & test[,j]=="A")+1, base=2)
			zt[14,i] <- log(sum(test[,i]=="C" & test[,j]=="X")+1, base=2)
			zt[15,i] <- log(sum(test[,i]=="C" & test[,j]=="G")+1, base=2)
			zt[16,i] <- log(sum(test[,i]=="C" & test[,j]=="C")+1, base=2)
			
							}
									}
  zt
}

din.diff <- din.freq (xf)-din.freq (xt)

#______________________________________________________#
# Encoding of test sequence after passing through difference matrix#
#_______________________________________________________#

encode <- function(k){
s <- unlist(strsplit(k, split=""))
s[s=="T"|s=="TRUE"]<- "X"
les <- length(s)
enc.tst <- vector(mode="numeric",length=les-1)

	for(j in 1:(les-1)){
        if(s[j]=="A" & s[j+1]=="A"){enc.tst[j]<- din.diff[1,j]}
        if(s[j]=="A" & s[j+1]=="X"){enc.tst[j]<- din.diff[2,j]}
        if(s[j]=="A" & s[j+1]=="G"){enc.tst[j]<- din.diff[3,j]}
        if(s[j]=="A" & s[j+1]=="C"){enc.tst[j]<- din.diff[4,j]}
      
        if(s[j]=="X" & s[j+1]=="A"){enc.tst[j]<- din.diff[5,j]}
        if(s[j]=="X" & s[j+1]=="X"){enc.tst[j]<- din.diff[6,j]}
        if(s[j]=="X" & s[j+1]=="G"){enc.tst[j]<- din.diff[7,j]}
        if(s[j]=="X" & s[j+1]=="C"){enc.tst[j]<- din.diff[8,j]}
      
        if(s[j]=="G" & s[j+1]=="A"){enc.tst[j]<- din.diff[9,j]}
        if(s[j]=="G" & s[j+1]=="X"){enc.tst[j]<- din.diff[10,j]}
        if(s[j]=="G" & s[j+1]=="G"){enc.tst[j]<- din.diff[11,j]}
        if(s[j]=="G" & s[j+1]=="C"){enc.tst[j]<- din.diff[12,j]}
      
        if(s[j]=="C" & s[j+1]=="A"){enc.tst[j]<- din.diff[13,j]}
        if(s[j]=="C" & s[j+1]=="X"){enc.tst[j]<- din.diff[14,j]}
        if(s[j]=="C" & s[j+1]=="G"){enc.tst[j]<- din.diff[15,j]}
        if(s[j]=="C" & s[j+1]=="C"){enc.tst[j]<- din.diff[16,j]}
		}
enc.tst
}
enc_seq <- t(sapply (zz, encode))
enc_seq
}
