#####################################
##### core function: pfa  ##########
#####################################

pfa <- function(Z, Sigma, t, Kmax, reg="L1", e=.05, gamma, K, plot="-log") {
  
  Z <- as.vector(Z)
  Sigma <- as.matrix(Sigma)
  p <- length(Z)
  
  # standardize the data

  SD <- sqrt(diag(Sigma))
  Z <- Z/SD
  Sigma <- diag(1/SD) %*% Sigma %*% diag(1/SD)

  # pca 

  pca <- svd(Sigma, nu=0, nv=Kmax)
  lambda <- pca$d
  eigvec <- pca$v
    

  # determine the factor loadings
  
  if (missing(K)) {
      K = 1
      while( K < Kmax & sqrt(sum(lambda[(K+1):length(lambda)]^2)) >= e*sum(lambda)) 
         K = K + 1 
  }    
  sqrt_lambda <- as.matrix(sqrt(lambda[1:K]))
  b <- as.matrix(eigvec[,1:K])
  for (i in 1:K)  {
    b[,i] <- b[,i] * sqrt_lambda[i]  # factor loadings
  }


  # estimate the factors, with 5% largest |Z| eliminated.
   
  
  if (reg == "L1") {
    W.hat <- rq(Z ~ b -1, 0.5)$coef     # L_1 regression (no intercept)
    # W.hat <- W.hat[2:(K+1)] 
  } 
  else if (reg == "L2")  # L_2 regression (no intercept) 
  {    
    #temp<-sort(abs(Z),decreasing=TRUE,index.return=TRUE)
    #len=round(length(Z)*0.05)
    #Ztemp<-temp$x
    #btemp<-as.matrix(b[temp$ix,])
    #Ztemp<-Ztemp[(len+1):length(Z)]
    #btemp<-btemp[(len+1):length(Z),]
    #W.hat<-lm(Ztemp ~ btemp - 1)$coef
    o = order(abs(Z))
    Zperm = Z[o]
    Lperm = as.matrix(b[o,])
    Z.reduce = Zperm[1:(p*0.95)]
    L.reduce = as.matrix(Lperm[1:(p*0.95),]) 
    W.hat = lsfit(x=L.reduce,y=Z.reduce,intercept=F)$coef
  }
  #if (reg == "huber") {
    #W.hat <- rlm(Z ~ b, 0.5)$coef	  # robust/huber regression
  #}

  rs <- rowSums(b^2)
  inv_a <- sqrt( ((1 - rs)+abs(1-rs))/2 )
  bW.est <- b%*%(W.hat)


  # compute p-values & estimate p0
  
  P <- 2*(1-pnorm(abs(Z)))
  sort <- sort(P, index.return=TRUE)
  index <- sort$ix
  P <- sort$x
  if (missing(gamma))  
      gamma <- as.numeric(quantile(P, probs=0.4))
  p0.est <- min(p, sum(P>gamma)/(1-gamma))


  # estimate FDP
  
  t.default <- TRUE
  if (!missing(t)) {
       if (t=="pval")  {
          t <- P
          t.default <- FALSE
       } 
       if (is.numeric(t)) 
          t.default = ( sum(t>=0)+ sum(t<=1) < 2*length(t))
  }
  if (t.default) {
       logt.l <- max(min(log(P)), log(1e-14))
       logt.u <- max(log(P))
       grid <- (logt.u-logt.l)*seq(from=0.01,to=1,by=0.025)*0.5 + 0.85*logt.l+0.15*logt.u
       t <- exp(grid)
  }
       
  FDPt <- Vt <- Rt<- rep(0, length(t))
  for (l in 1:length(t)) {
  	   P1 <- 2*(1-pnorm(abs(Z)))
       Rt[l] <- sum(P1<=t[l])
       a <- rep(0,p)
       for (j in 1:p)  {
           qtl <- qnorm(t[l]/2)
           if (inv_a[j]>0)  {
                a[j] <- pnorm((qtl + bW.est[j])/inv_a[j])+ pnorm((qtl - bW.est[j])/inv_a[j])
           } else {
                a[j] <- as.numeric(abs(bW.est[j])>abs(qtl))
           }
       }
       Vt[l] <- min(sum(a), Rt[l]) 
       if (Rt[l]==0)   {
           FDPt[l] <- 0
       } else  {
           FDPt[l] <- Vt[l]/Rt[l]
       }
  } 


  # factor adjusted procedure

  adj.P <- as.vector(rep(0,p))
  for (j in 1:p) {
       if (inv_a[j]>0)  {
           adj.P[j] <- 2*( 1- pnorm(abs(Z[j] - bW.est[j])/inv_a[j]) )
       }  else  {
           adj.P[j] <- as.numeric(abs(Z[j]-bW.est[j])==0)
       }
  }
  sort <- sort(adj.P, index.return=TRUE)
  adj.index <- sort$ix
  adj.P <- sort$x

  
  # output
  
  Pvals <- data.frame(p.value = P,  Index =index)
  adjPvals <- data.frame(p.value = adj.P,  Index = adj.index)
  if (t.default)   {
       FDPvals <- data.frame( minus.logt= - log(t), rejects= Rt, false.rejects=Vt, FDP=FDPt )
  } else {
       FDPvals <- data.frame( t= t, rejects= Rt, false.rejects=Vt, FDP=FDPt )
  }
  results <- list("Pvalue"=Pvals, "adjPvalue"=adjPvals, "FDP"=FDPvals, "pi0"=p0.est/p, "K"=K, "sigma"=NULL)
  class(results) <- "FDPresults"
  if (plot=="-log")  {
            par(mfrow=c(2,2))
            hist(P, main = "Histogram of p-values", xlab="p-values")
            plot(-log(t), Rt, xlab="-log(t)", ylab="", main="Number of total rejections", type='o')
            plot(-log(t),Vt, xlab="-log(t)", ylab="", main="Number of estimated false rejections", type='o')
            plot(-log(t),FDPt,xlab="-log(t)", ylab="", main="Estimated FDP", type='o')
   }  else  if (plot=="linear") {
            par(mfrow=c(2,2))
            hist(P, main = "Histogram of p-values", xlab="p-values")
            plot(t, Rt, xlab="t", ylab="", main="Number of total rejections", type='o')
            plot(t,Vt, xlab="t", ylab="", main="Number of estimated false rejections", type='o')
            plot(t,FDPt,xlab="t", ylab="", main="Estimated FDP", type='o')
  }  else if (plot=="log") {
            par(mfrow=c(2,2))
            hist(P, main = "Histogram of p-values", xlab="p-values")
            plot(log(t), Rt, xlab="log(t)", ylab="", main="Number of total rejections", type='o')
            plot(log(t),Vt, xlab="log(t)", ylab="", main="Number of estimated false rejections", type='o')
            plot(log(t),FDPt,xlab="log(t)", ylab="", main="Estimated FDP", type='o')
  }
  return(results)
}


#########################################
##### core function: print.FDPresults ###
#########################################

print.FDPresults <- function(x, ...)  {

  cat("P-values:\n")
  print(x$Pvalue)
  
  cat("Factor-ajusted P-values:\n")
  print(x$adjPvalue)

  cat("Estimated false discoveries:\n")
  print(x$FDP)
  
  cat("Estimated true null proportion:\n")
  print(x$pi0)

  cat("Number of factors:\n")
  print(x$K)

  if (! is.null(x$sigma))  {
      cat("Estimated noise standard deviation:\n")
      print(x$sigma)
  }
}


###########################################
##### core function: summary.FDPresults ###
###########################################

summary.FDPresults <- function(object, ...)  { 
  x <- object

  cat("Method:\n")
  cat(paste("PFA with", x$K, "factors.\n"))
  cat("\n")
  
  p <- length(x$Pvalue$p.value)
  cat("P-values:\n")
  print(x$Pvalue[1:min(20,p),])
  cat("...\n")
  
  cat("Factor-ajusted P-values:\n")
  print(x$adjPvalue[1:min(20,p),])
  cat("...\n")

  T <- length(x$FDP$rejects)
  cat("Estimated false discoveries:\n")
  print(x$FDP[1:min(10,T),])
  cat("...\n")
  
  cat("Estimated true null proportion:\n")
  cat(paste(round(x$pi0, digits=3), "\n"))

  if (! is.null(x$sigma))  {
      cat("Estimated noise standard deviation:\n")
      cat(paste(round(x$sigma,digits=3), "\n"))
  }
}  





