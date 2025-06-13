PredCRG_Enc <-
function(prot_seq){
x1 <- prot_seq
nam <- names(x1)
dat_nam <- paste(substr(nam,1,20),"...", sep="")
x <- as.character(as.character(x1))
############################################CTDT#################################
my_ctdt <- function(s){
  z <- unlist(extractCTDT(s))
  z
}
ctdt <- t(sapply(x,my_ctdt))

##########################################FASGAI#################################
my_fasgai <- function(s){
  z <- unlist(fasgaiVectors(s))
  z
}
fasgai <- t(sapply(x,my_fasgai))
################hydrophobicity, instaindex, molecular weight, isoelcpoint################

hydro <- sapply(x, hydrophobicity)
Insta <- sapply(x, instaIndex)
Mw <- sapply(x, mw)
Iep <- sapply(x, pI)

######################cruciani properties###############
my_crucini <- function(s){
  z <- unlist(crucianiProperties(s))
  z
}
crucini <- t(sapply(x, my_crucini))

###################protfp#################
my_protfp <- function(s){
  z <- unlist(protFP(s))
  z
}
protfp <- t(sapply(x, my_protfp))
##########################Physichemical properties###############

phyc <- cbind(hydro, Insta, Mw, Iep, fasgai, crucini, protfp)

#########################aminoacid composition##############

aac <- t(sapply(x, extractAAC))

########################Full features################

new_dat <- as.matrix(cbind(phyc, aac, ctdt))
#colnames(new_dat) <- paste(rep("V", 62), 2:63, sep="")


zx <- width(x1)


if(length(zx)==1){
  test1 <- matrix(new_dat, ncol=62, nrow=1)
}else{
  test1 <- as.matrix(new_dat)
}
return(test1)
}
