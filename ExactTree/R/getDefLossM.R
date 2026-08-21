getDefLossM<-function(NCat){
#LossM(i,j) = loss matrix for incorrectly classifying i as j
#The default loss matrix counts each misclassification as 1, diag=0
LossM <- -diag(NCat)+1

return(LossM)
}
