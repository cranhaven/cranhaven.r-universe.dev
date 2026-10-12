enhcHi<-function(data,En=100,len=c(2,10),type=2){
  LL<-dim(data)
  Lq<-LL[2]
  XO0 <- data
  j <- 1
  CC <- NULL
  for (j in 1:En) {
    K0 <- sample(len[1]:len[2], 1)
    r01 <- unique(sample(Lq, replace = TRUE))
    if(type==2) r01 <- unique(sample(r01, replace = TRUE))
    #r01 <- unique(sample(r01, replace = TRUE))
    XO1 <- XO0[, c(r01)]
    CC0 <- HCALB(XO1, K0)
    CC <- cbind(CC, CC0)
    j <- j + 1
  }
  return(CC)
}

