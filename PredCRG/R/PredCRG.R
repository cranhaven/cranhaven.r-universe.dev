globalVariables(c("model1", "model2", "model3","model4"))
PredCRG <-
function(seq_data){

x1 <- seq_data
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
lx <- length(x1)

result_label <- vector(mode="character", length=lx)
result_prob <- vector(mode="numeric", length=lx)

zx <- width(x1)
zx1 <- which(zx<221)
zx2 <- which(zx>=221 & zx<363)
zx3 <- which(zx>=363 & zx<539)
zx4 <- which(zx>=539)


if(length(zx1)==1){
  test1 <- matrix(new_dat[zx1,], ncol=62, nrow=1)
}else{
  test1 <- as.matrix(new_dat[zx1,])
}

if(!length(zx1)==0){
#data("PredCRG_model")
  #load("Q1_laplace.rda")  
  pred <- predict(model1, newdata=test1, type="response")
  pred1 <- predict(model1, newdata=test1, type="probabilities")
  result_label[zx1] <- as.character(pred)
  result_prob[zx1] <- pred1[,"CRG"]
}

if(length(zx2)==1){
  test2 <- matrix(new_dat[zx2,], ncol=62, nrow=1)
}else{
  test2 <- as.matrix(new_dat[zx2,])
}

if(!length(zx2)==0){
#data(model2)
 #load("Q2_laplace.rda")  
  pred <- predict(model2, newdata=test2, type="response")
  pred1 <- predict(model2, newdata=test2, type="probabilities")
  result_label[zx2] <- as.character(pred)
  result_prob[zx2] <- pred1[,"CRG"]
}


if(length(zx3)==1){
  test3 <- matrix(new_dat[zx3,], ncol=62, nrow=1)
}else{
  test3 <- as.matrix(new_dat[zx3,])
}


if(!length(zx3)==0){
#data(model3)
  #load("Q3_laplace.rda")  
  pred <- predict(model3, newdata=test3, type="response")
  pred1 <- predict(model3, newdata=test3, type="probabilities")
  result_label[zx3] <- as.character(pred)
  result_prob[zx3] <- pred1[,"CRG"]
}

if(length(zx4)==1){
  test4 <- matrix(new_dat[zx4,], ncol=62, nrow=1)
}else{
  test4 <- as.matrix(new_dat[zx4,])
}

if(!length(zx4)==0){
#data(model4)
  #load("Q4_laplace.rda")  
  pred <- predict(model4, newdata=test4, type="response")
  pred1 <- predict(model4, newdata=test4, type="probabilities")
  result_label[zx4] <- as.character(pred)
  result_prob[zx4] <- pred1[,"CRG"]
}

data.frame(Seq_name=dat_nam,Predicted_label=result_label, Predicted_probability=result_prob)

}
