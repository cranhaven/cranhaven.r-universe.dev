Density.Feature <-
function(test_seq){
if(class(test_seq)!="DNAStringSet"){stop("The dataset must be of class DNAStringSet")}
if(length(unique(width(test_seq)))>1){stop("Each sequence must of equal length")}
zz <- as.character(as.character(test_seq))

#########################################################density encoding##################
my.den <- function (dat){
ss <- unlist(strsplit(dat, split=""))
ss[ss=="T"|ss=="TRUE"]<- "X"
z <- vector(mode="numeric", length=length(ss))
IA <- as.numeric(which(ss=="A"))
lA <- length(IA)
seA <- sequence (lA)
z[IA] <- seA/IA
#__________________________________ 
IT <- as.numeric(which(ss=="X"))
lT <- length(IT)
seT <- sequence (lT)
z[IT] <- seT/IT
#__________________________________
IG <- as.numeric(which(ss=="G"))
lG <- length(IG)
seG <- sequence (lG)
z[IG] <- seG/IG
#__________________________________
IC <- as.numeric(which(ss=="C"))
lC <- length(IC)
seC <- sequence (lC)
z[IC] <- seC/IC
z
}

x1 <- round(t(sapply(zz, my.den)),3)
return(x1)
}
