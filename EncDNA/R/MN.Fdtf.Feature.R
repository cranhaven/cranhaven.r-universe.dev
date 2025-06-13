MN.Fdtf.Feature <-
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
mmt <- nt_mn/nr
#_______________________________________________________#
nl <- ncol(xf)
nr <- nrow(xf)
nf_mn <- t(sapply (ind_mn, function (z) apply(xf, 2, function (s) countmn (s,z) )))
mmf <- nf_mn/nr
#________________________________________________________________#
mm1 <- round(mmf-mmt,3)
#_______________________________________________________________#
zz <- as.character(as.character(test_seq))
encode <- function(k){
s <- unlist(strsplit(k, split=""))
s[s=="T"|s=="TRUE"]<- "X"
les <- length(s)
z <- vector(mode="numeric", length=les)
aa <- which(s=="A")
z[aa] <- mm1[1,aa]
at <- which(s=="X")
z[at] <- mm1[2,at]
ag <- which(s=="G")
z[ag] <- mm1[3,ag]
ac <- which(s=="C")
z[ac] <- mm1[4,ac]
z
}

mn_fdtf <- t(sapply(zz, encode))
mn_fdtf
}
