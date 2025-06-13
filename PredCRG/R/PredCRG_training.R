PredCRG_training <-
function(pos_seq, neg_seq, kern){

PredCRG_Enc <-function(prot_seq){
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


 test1 <- as.matrix(new_dat)
 return(test1)
}


pos1 <- PredCRG_Enc(pos_seq)
rownames(pos1)<- NULL
neg1 <- PredCRG_Enc(neg_seq)
rownames(neg1)<- NULL

pos_y1 <- rep("CRG", nrow(pos1))
neg_n1 <- rep("non-CRG", nrow(neg1))

y1 <- as.factor(c(pos_y1, neg_n1))


colnames(pos1)<-colnames(neg1)<- paste(rep("V", 62), 2:63, sep="")
dat <- rbind(pos1, neg1)

if (kern=="laplace"){
model <- ksvm(x=dat, y=y1, kernel="laplacedot", prob.model=TRUE)
}

if (kern=="linear"){
model <- svm(x=dat, y=y1, kernel="linear", probability=TRUE)
}

if (kern=="polynomial"){
model <- svm(x=dat, y=y1, kernel="polynomial", probability=TRUE)
}

if (kern=="RBF"){
model <- svm(x=dat, y=y1, kernel="radial", probability=TRUE)
}


return(model)
}
