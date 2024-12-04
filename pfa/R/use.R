require(quantreg)
require(lars)
require(POET)


#####################################
##### user function 1: pfa.test #####
#####################################

pfa.test <- function(X, Y, tval, Sigma, reg="L2", K, e=0.05, gamma, mat_est="poet", plot="-log") {


# compute the test statistics

if (missing(Y)) {
    X <- as.matrix(X)
    if (ncol(X)==1)  {
      Z <- as.vector(X)
    }  else  {
      Z <- colMeans(X)*sqrt(nrow(X))
    }
} else {
    X <- as.matrix(X)
    Y <- as.matrix(Y)
    if (ncol(X)!= ncol(Y))
      stop("Dimensions of X and Y do not match.")
    Z <- (colMeans(X) - colMeans(Y))*sqrt(nrow(X)*nrow(Y)/(nrow(X)+nrow(Y)))
}
p <- length(Z)
Kmax <- p

# determine number of factors
if (missing(K)&(ncol(X)>=2)) {
	if(missing(Y)){
	  kmax<-floor(0.2*nrow(X))
	  if(kmax<=1){
		stop("No enough samples to determine the number of factors!")
	   }else{
      sample.cor<-cor(t(X),t(X))
      sample.pca<-eigen(sample.cor)
      sample.eigval<-sample.pca$values
      ratio<-sample.eigval[1:(kmax-1)]/sample.eigval[2:kmax]
      K<-which(ratio==max(ratio))	
       }
    }else{
    	  n<-nrow(X)
    	  m<-nrow(Y)
    	  kmax<-floor(0.2*(n+m))
    	  if(kmax<=1){
    	  	stop("No enough samples to determine the number of factors!")
    	  }else{
    	    Xmean<-apply(X,2,mean)
    	    Ymean<-apply(Y,2,mean)
    	    Znew<-as.matrix(rbind(X-Xmean,Y-Ymean))
    	    sample.cor<-cor(t(Znew),t(Znew))
        sample.pca<-eigen(sample.cor)
        sample.eigval<-sample.pca$values
        ratio<-sample.eigval[1:(kmax-1)]/sample.eigval[2:kmax]
        K<-which(ratio==max(ratio))
      }
    }
}


# compute the covariance matrix

if (missing(Sigma)) {
    if (missing(Y) & nrow(X)==1) {
       stop("No enough samples to estimate the covariance matrix.")
    } 
    if (mat_est =="sample") {
       n <- nrow(X)
       barX <- matrix(colMeans(X), nrow=n, ncol=p, byrow=T)
       SigmaX <- (t(X - barX )%*% (X - barX))/n
       if (missing(Y)) {
          Sigma <- SigmaX
          Kmax <- n
       } else {
          n2 <- nrow(Y)
          barY <- matrix(colMeans(Y), nrow=n2, ncol=p, byrow=T)
          SigmaY <- (t(Y - barY)%*% (Y - barY))/n2
          Sigma <- (SigmaX*n + SigmaY*n2)/(n+n2)
          Kmax <- n + n2
       }  
     } else { 
          if (mat_est=="poet") {
             n <- nrow(X)
             barX <- matrix(colMeans(X), nrow=n, ncol=p, byrow=T)
             if (missing(Y)) {
                # R <- X - barX
                R<-X
             } else {
                n2 <- nrow(Y)
                barY <- matrix(colMeans(Y), nrow=n2, ncol=p, byrow=T)
                # R <- rbind(X-barX, Y-barY)
                R<-rbind(X,Y)
             }
             Sigma <- POET(t(R),K=K,C=0.5,thres='soft',matrix='vad')$SigmaY
             Kmax <- p
          }
     }
}

# run the test and compute FDP

results <- pfa(Z=Z, Sigma=Sigma, t=tval, Kmax=Kmax, reg=reg, e=e, gamma=gamma, K=K, plot=plot)
return(results)

}


#####################################
##### user function 2: pfa.gwas #####
#####################################


pfa.gwas <- function(X,Y, tval, v, reg="L1", e=0.05, gamma, K, plot="-log") {

    X <- as.matrix(X)
    Y <- as.matrix(Y)
    if (nrow(X)!=nrow(Y) | ncol(Y)!=1) 
      stop("Dimensions do not match.")
    n <- nrow(X)
    p <- ncol(X)
    
    # compute Z and Sigma

    Z <- rep(0, p)
    for (j in 1:p)  {    # marginal regression
        Z[j] <- lm(Y ~ X[,j])$coef[2]
    }
    if (missing(v)) {
        sigma.est <- rcv(X,Y)     # refitted cross validation
    }  else {
        sigma.est <- v
    }
    barX <- matrix(colMeans(X), nrow=n, ncol=p, byrow=T)
    R <- (t(X-barX)%*% (X-barX))/n 
    Sigma <- matrix(rep(0, p*p), nrow=p)
    for (i in 1:p) {
        for (j in i:p)  {
             Sigma[i,j] <- Sigma[j,i] <- R[i,j]*(sigma.est)^2/R[i,i]/R[j,j]/n
        }
    }
    
    # run the test and compute FDP
    
    results <- pfa(Z, Sigma, t=tval, Kmax=n, reg=reg, e=e, gamma=gamma, K=K, plot=plot)
    results$Pvalue <- data.frame(coefficient = Z[results$Pvalue$Index], p.value = results$Pvalue$p.value,  Index =results$Pvalue$Index)
    results$sigma <- sigma.est
    return(results)
}



# refitted cross validation

rcv <- function(x, y) {
  n <- nrow(x)
  p <- ncol(x)
  # dimension reduction by removing ultra-highly correlated variables
  # C <- abs(cor(x,x)) 
  # correlation matrix
  
  barx<-colMeans(x)
  C<-t(x - barx)%*% (x - barx)/(n-1)
  for (i in 1:p) {
     C[i,] <- C[i,]/sqrt(C[i,i])
    }
    for (j in 1:p) {
     C[,j] <- C[,j]/sqrt(C[j,j])
    }
  C <- C*upper.tri(C) # all values under diagonal to 0
  if(nrow(C) > 0) {
  	if(sum(C>0.98)!=0){
    cols <- which(C > 0.98, arr.ind=T)
    cols <- unique(cols[,2])
    x <- x[,-cols]
    }
  }
  sigma_sq <- sigma1_sq <- sigma2_sq <- rep(0,30)
  for (i in 1:30){
    count <-1
    repeat {
      seq <- c(1:n)
      seq <- sample(seq)
      seq1 <- seq[1:(n/2)]
      seq2 <- seq[((n/2)+1):n]	# random split of the data into two halves
      y1 <- y[seq1]
      X1 <- x[seq1,]
      y2 <- y[seq2]
      X2 <- x[seq2,]
      try(result1<-lars(X1,y1,type="lasso", normalize=FALSE, use.Gram=FALSE),silent=TRUE)# LASSO algorithm
      try(result2<-lars(X2,y2,type="lasso", normalize=FALSE, use.Gram=FALSE),silent=TRUE)# LASSO algorithm
      if(exists("result1") & exists("result2"))
        break
      count <- count+1
      if(count>1000)
        stop("Refitted cross validation fails. Please input v.")
    }

    # refitted cross-validation (RCV)
    if(p<200){
       cvres1 <- cv.lars(X1, y1, K=5,plot.it=FALSE,type="lasso")	  
       sBest1<-cvres1$index[which.min(cvres1$cv)]
       cres1<-predict.lars(result1,type="coefficients",s=sBest1,mode="fraction")
       c1<-cres1$coefficient
    } else{
       c1<-coef(result1)[10,]
    }
    if(p<200){
       cvres2 <- cv.lars(X2, y2, K=5, plot.it=FALSE,type="lasso")	  
       sBest2<-cvres1$index[which.min(cvres2$cv)]
       cres2<-predict.lars(result2,type="coefficients",s=sBest2,mode="fraction")
       c2<-cres2$coefficient
    } else{
       c2<-coef(result2)[10,]
    }
      
    
    ind1<-which(c1!=0)
    if (length(ind1)!=0){
    #ind1 <- (c1[10,]!=0)	# specifying model size
    resid1 <- lsfit(X2[,ind1],y2)$resid
    sigma1_sq[i] <- sum(resid1^2)/(n/2 - length(ind1))
    }   else{
    sigma1_sq[i] <- sum(y2^2)/(n/2) }
    
    
    #ind2 <- (c2[10,]!=0)
    
    ind2<-which(c2!=0)
    if(length(ind2)!=0){
    resid2 <- lsfit(X1[,ind2],y1)$resid
    sigma2_sq[i] <- sum(resid2^2)/(n/2 - length(ind2))
    }  else{
    	sigma2_sq[i] <- sum(y1^2)/(n/2)
    	}
    sigma_sq[i] <- (sigma1_sq[i] + sigma2_sq[i])/2	   # RCV-estimated variance
  }
  sigmahat=sqrt(mean(sigma_sq[sigma_sq!=0]))
  return(sigmahat)
}


  






