MM1.Feature <-
function(positive_class, test_seq){
if(class(positive_class)!="DNAStringSet"){stop("The positive_class must be of class DNAStringSet")}
if(length(unique(width(positive_class)))>1){stop("Each sequence of positive_class must of equal length")}

if(class(test_seq)!="DNAStringSet"){stop("The test_seq must be of class DNAStringSet")}
if(length(unique(width(test_seq)))>1){stop("Each sequence of test_seq must be of equal length")}
zz <- as.character(as.character(test_seq))

x <- as.matrix(positive_class)
x[x=="T" | x=="TRUE"]<-"X"
nl <- ncol(x)
nr <- nrow(x)
x1 <- x[,1:(nl-1)]
x2 <- x[,2:nl]
p <- paste(x1, x2, sep="")
mp <- matrix(p, nrow=nr, ncol=(nl-1))

countdi <- function(s, z) {
k <- sum((s==z)*1)
k
}
ind_nu <- c("A","X","G","C")
ind_di <- c("AA","AX","AG","AC", "XA","XX","XG","XC", "GA","GX","GG","GC", "CA","CX","CG","CC")
n_di <- t(sapply (ind_di, function (z) apply(mp, 2, function (s) countdi (s,z) )))
n_nu <- t(sapply (ind_nu, function (z) apply(x1, 2, function (s) countdi (s,z) )))
dr <- apply(n_nu, 2, function(r) rep(r,each=4))
mm1 <- round(n_di/(dr+0.000001),3)

encode <- function(k){
s <- unlist(strsplit(k, split=""))
s[s=="T"|s=="TRUE"]<- "X"
les <- length(s)
z <- vector(mode="numeric", length=(les-1))

z1 <- s[1:(les-1)]
z2 <- s[2:les]
pz <- paste(z1, z2, sep="")
aa <- which(pz=="AA")
z[aa] <- mm1[1,aa]
at <- which(pz=="AX")
z[at] <- mm1[2,at]
ag <- which(pz=="AG")
z[ag] <- mm1[3,ag]
ac <- which(pz=="AC")
z[ac] <- mm1[4,ac]

ta <- which(pz=="XA")
z[ta] <- mm1[5,ta]
tt <- which(pz=="XX")
z[tt] <- mm1[6,tt]
tg <- which(pz=="XG")
z[tg] <- mm1[7,tg]
tc <- which(pz=="XC")
z[tc] <- mm1[8,tc]

ga <- which(pz=="GA")
z[ga] <- mm1[9,ga]
gt <- which(pz=="GX")
z[gt] <- mm1[10,gt]
gg <- which(pz=="GG")
z[gg] <- mm1[11,gg]
gcc <- which(pz=="GC")
z[gcc] <- mm1[12,gcc]

ca <- which(pz=="CA")
z[ca] <- mm1[13,ca]
ct <- which(pz=="CX")
z[ct] <- mm1[14,ct]
cg <- which(pz=="CG")
z[cg] <- mm1[15,cg]
cc <- which(pz=="CC")
z[cc] <- mm1[16,cc]
z
}

enc <- t(sapply(zz, encode))
enc

}
