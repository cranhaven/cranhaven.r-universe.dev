SR_test_boots <-  function(xf = xf, listw = listw, nv = nv){

  ###

  y <- xf
  q <- max(y)
  n <- length(y)
  # Cont is a binary variable that takes on the value of 1 if data are
  # continuous and 0 if data are categorical.

  m <- numeric() # matrix(0,nrow=q,ncol=1)
  pprod <- numeric() # matrix(0,nrow=q,ncol=1)
  for (i in 1:q){
    m[i] <- sum(y==i)
    pprod[i]<- m[i]*(n-m[i])
  }

  if (inherits(listw, "knn")){
    lnnb <- matrix(dim(listw$nn)[2],ncol = 1,nrow = dim(listw$nn)[1])}
  if (inherits(listw, "nb")){
    lnnb <- rowSums(nb2mat(listw, style = 'B',
                                  zero.policy = TRUE))
  }

  # here we categorize the original data set y into the q categories
  # compute the m_k needed for the computation of mean and variance
  # pprod is needed for the computation of p
  p=sum(pprod)/(n*(n-1))


  ##### COMPUTING THE VARIANCE #####
  ## case 1 ##
  aux1 <- numeric()
  aux31 <- numeric()
  aux3 <- numeric()

  t1=0;
  for (k in 1:q){
    for (c in 1:q){
      t1=t1+1
      aux1[t1]=m[k]*m[c]*(n-m[c]-1)
      aux31[t1]=m[k]*m[c]*((m[k]-1)*(n-m[k]-1)+(m[c]-1)*(n-m[c]-1))
      if(k==c){
        aux1[t1]=0
        aux31[t1]=0
      }
    }
  }

  t3=0
  aux3<-numeric()
  for (k in 1:q){
    for (c in 1:q){
      for (d in 1:q){
        t3=t3+1
        aux3[t3] <- m[k]*m[c]*m[d]*(n-m[d]-2)
        if (c==k){aux3[t3]=0}
        if (d==k){aux3[t3]=0}
        if (d==c){aux3[t3]=0}
      }
    }
  }

  var1 <- 1/(n*(n-1)*(n-2)*(n-3))*(sum(aux3)+sum(aux31));
  var2 <- 1/(n*(n-1)*(n-2))*sum(aux1);
  var3 <- p;

  varSR <-p*(1-p)*sum(lnnb)+nv[1]*var1+nv[2]*var2+nv[3]*var3-(nv[1]+nv[2]+nv[3])*p^2

  # Here we compute the runs starting at each location and it sum is the total number of runs
  nruns <- matrix(0,ncol = 1,nrow = n)
  for (i in 1:n){
    if (lnnb[i]!= 0){ # Solo calcula los test locales si el elemento tiene vecinos
      if (inherits(listw, "knn")){
        runs <- y[c(i,listw$nn[i,])]}
      if (inherits(listw, "nb")){
        runs <- y[c(i,listw[[i]])]}
      nruns[i] <- 1 + sum(abs(diff(runs))>0)
    }
  }
  #
  SR=sum(nruns)
  #The mean of the statistic
  meanSR=n+p*sum(lnnb)

  # # The SRQ global test statistic which is N(0,1) distributed
  SRQ=(SR-meanSR)/sqrt(varSR)
  # p.valueSRQ <- 2*(1 - pnorm(abs(SRQ), mean = 0, sd = 1))
  return <- list(SR=SR, SRglobal = SRQ, nruns = nruns)
}
