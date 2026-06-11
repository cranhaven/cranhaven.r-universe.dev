LP.basis <-
function(p.dist,m){
   fm <- cumsum(p.dist) - .5*p.dist  #--Fmid
   sdfmid <- sqrt(wt.mean(fm^2,p.dist) - (wt.mean(fm,p.dist))^2)
   m <- min(length(p.dist)-1, m )
   T1 <- (fm- wt.mean(fm,p.dist))/sdfmid  #--first score function
   T <-  as.matrix(stats::poly(T1,m,raw=TRUE))

    ##--Gram-Schmidt Orthonormalization (with proper weights)
   v <- u<- T
   p <- nrow(u)  # dimension of the space
   n <- ncol(u)  # number of vectors
   for (i in 2:n){
      coef.proj <- c(car::wcrossprod(u[,i],v[,1:(i-1)],  w=p.dist)  ) / 
          diag(car::wcrossprod(v[,1:(i-1)],   w=p.dist)    )
      v[,i] <- u[,i] - matrix(v[,1:(i-1)],nrow=p)%*%matrix(coef.proj,nrow=i-1)
      v[,i] <- v[,i] - wt.mean(v[,i], p.dist)
   }
   coef.proj <- 1/sqrt(diag(car::wcrossprod(v, w=p.dist)))
   v <- t(t(v) * coef.proj)
   return(v)
}
