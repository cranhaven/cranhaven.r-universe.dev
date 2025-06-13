WMM.Feature <-
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

#________________________________________________________#
countmn <- function(s, z) {
k <- sum((s==z)*1)
k
}
ind_mn <- c("A","X","G","C")
#_______________________________________________________#
nl <- ncol(xt)
nr <- nrow(xt)
nt_mn <- t(sapply (ind_mn, function (z) apply(xt, 2, function (s) countmn (s,z) )))
pos_tss <- nt_mn/nr
#_______________________________________________________#
nl <- ncol(xf)
nr <- nrow(xf)
nf_mn <- t(sapply (ind_mn, function (z) apply(xf, 2, function (s) countmn (s,z) )))
pos_fss <- nf_mn/nr

#___________________________________________________#
#Replacing the true and false acceptor splice site sequence with TSS PWM#
#___________________________________________________#
zz <- as.character(as.character(test_seq))
encode_wmm <- function(k){
s <- unlist(strsplit(k, split=""))
les <- length(s)
s[s=="A"] <- 1
s[s=="T"|s=="TRUE"]<- 2
s[s=="G"] <- 3 
s[s=="C"] <- 4

mt <- vector(mode="numeric", length=les)
mf <- vector(mode="numeric", length=les)

for(i in 1:les){
mt[i] <- pos_tss[as.numeric(s[i]),i]
mf[i] <- pos_fss[as.numeric(s[i]),i]
}
WMM_t <- sum(log(mt,base=2))
WMM_t[WMM_t==-Inf]<-0
WMM_f <- sum(log(mf,base=2))
DWMM <- WMM_t - WMM_f
DWMM[DWMM==-Inf]<-0
c(WMM_t, DWMM)
}

wmm <- t(sapply(zz, encode_wmm))
wmm
}
