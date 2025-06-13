Bayes.Feature <-
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

#############################################
#Finding position weight profile of bothe TSS and FSS from the training set
####################################################
pwm <- function(s){
la <- length(which(s=="A"))
lx <- length(which(s=="X"))
lg <- length(which(s=="G"))
lc <- length(which(s=="C"))
c <- c(la,lx,lg,lc)
c
}

pwm_t <- round(apply(xt,2,pwm)/nrow(xt),3)
pwm_f <- round(apply(xf,2,pwm)/nrow(xf),3)

#################################################
#Encoding of test sequence based on the PWM of true and false sites
################################################
zz <- as.character(as.character(test_seq))
encod_t <- function(s){
k <- unlist(strsplit(s, split=""))
k[k=="T"|k=="TRUE"]<- "X"
k[which(k=="A")]<- pwm_t[1,which(k=="A")]
k[which(k=="X")]<- pwm_t[2,which(k=="X")]
k[which(k=="G")]<- pwm_t[3,which(k=="G")]
k[which(k=="C")]<- pwm_t[4,which(k=="C")]
k
}

encod_f <- function(s){
k <- unlist(strsplit(s, split=""))
k[k=="T"|k=="TRUE"]<- "X"
k[which(k=="A")]<- pwm_f[1,which(k=="A")]
k[which(k=="X")]<- pwm_f[2,which(k=="X")]
k[which(k=="G")]<- pwm_f[3,which(k=="G")]
k[which(k=="C")]<- pwm_f[4,which(k=="C")]
k
}
# Encoding of TSS based on PWM of both TSS and FSS
en_seq_t <- matrix(as.numeric(t(sapply(zz, encod_t))), nrow=length(zz), ncol=nchar(zz[1]))
en_seq_f <- matrix(as.numeric(t(sapply(zz, encod_f))), nrow=length(zz), ncol=nchar(zz[1]))

return(cbind(en_seq_t,-en_seq_f))

}
