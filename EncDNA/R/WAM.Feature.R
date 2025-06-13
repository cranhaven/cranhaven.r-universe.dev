WAM.Feature <-
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
#________________________________________________________#
countdi <- function(s, z) {
k <- sum((s==z)*1)
k
}
ind_di <- c("AA","AX","AG","AC", "XA","XX","XG","XC", "GA","GX","GG","GC", "CA","CX","CG","CC")
#_______________________________________________________#
nl <- ncol(xt)
nr <- nrow(xt)
x1 <- xt[,1:(nl-1)]
x2 <- xt[,2:nl]
p <- paste(x1, x2, sep="")
mpt <- matrix(p, nrow=nr, ncol=(nl-1))
nt_di <- t(sapply (ind_di, function (z) apply(mpt, 2, function (s) countdi (s,z) )))
mmt <- nt_di/(nr+0.00001)
#_______________________________________________________#
nl <- ncol(xf)
nr <- nrow(xf)
x1 <- xf[,1:(nl-1)]
x2 <- xf[,2:nl]
p <- paste(x1, x2, sep="")
mpf <- matrix(p, nrow=nr, ncol=(nl-1))
nf_di <- t(sapply (ind_di, function (z) apply(mpf, 2, function (s) countdi (s,z) )))
mmf <- nf_di/(nr+0.00001)
#___________________________________________________#
#Replacing the true and false acceptor splice site sequence with TSS PWM#
#___________________________________________________#
zz <- as.character(as.character(test_seq))
encode_wam <- function(k){
s <- unlist(strsplit(k, split=""))
nl <- length(s)
s[s=="T"|s=="TRUE"]<- "X"
z1 <- s[1:(nl-1)]
z2 <- s[2:nl]
mpz <- paste(z1, z2, sep="")

mpz[mpz=="AA"] <- 1
mpz[mpz=="AX"] <- 2
mpz[mpz=="AG"] <- 3
mpz[mpz=="AC"] <- 4

mpz[mpz=="XA"] <- 5
mpz[mpz=="XX"] <- 6
mpz[mpz=="XG"] <- 7
mpz[mpz=="XC"] <- 8

mpz[mpz=="GA"] <- 9
mpz[mpz=="GX"] <- 10
mpz[mpz=="GG"] <- 11
mpz[mpz=="GC"] <- 12

mpz[mpz=="CA"] <- 13
mpz[mpz=="CX"] <- 14
mpz[mpz=="CG"] <- 15
mpz[mpz=="CC"] <- 16

mmz_t <- vector(mode="numeric", length=nl-1)
mmz_f <- vector(mode="numeric", length=nl-1)

for(i in 1:(nl-1)){
mmz_t[i] <- mmt[as.numeric(mpz[i]),i]
mmz_f[i] <- mmf[as.numeric(mpz[i]),i]
}

WAM_t <- sum(log(mmz_t,base=2))
WAM_f <- sum(log(mmz_f,base=2))

WAM_t[WAM_t==-Inf]<-0
DWAM <- WAM_t - WAM_f
DWAM[DWAM==-Inf]<-0
c(WAM_t, DWAM)
}

wam <- t(sapply(zz, encode_wam))
wam
}
