Sparse.Feature <-
function(test_seq){
if(class(test_seq)!="DNAStringSet"){stop("The dataset must be of class DNAStringSet")}
if(length(unique(width(test_seq)))>1){stop("Each sequence must of equal length")}
zz <- as.character(as.character(test_seq))

#########################################################sparse encoding########################
my.sparse <- function(dat){
ss <- unlist(strsplit(dat, split=""))
ss[ss=="T"|ss=="TRUE"]<- "X"
nx <- nchar(dat)
ss[which(ss=="A")] <- "111"
ss[which(ss=="X")] <- "001"
ss[which(ss=="G")] <- "100"
ss[which(ss=="C")] <- "010"
tsk <- as.numeric(unlist(strsplit(ss, split="")))
spr_enc <- matrix(tsk, ncol=nx*3, byrow=T)
spr_enc
}
########################################################Combining features###########################
x2 <- t(sapply(zz, my.sparse))
return(x2)

}
