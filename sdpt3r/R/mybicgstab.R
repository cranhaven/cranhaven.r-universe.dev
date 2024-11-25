mybicgstab <- function(A,b,M1,tol=1e-10,maxit=c(),printlevel=1){
  
  N <- length(b)
  if(length(maxit) == 0){
    if(!is.null(A$mat22)){
      maxdim <- max(dim(as.matrix(A$mat22)))
    }else{
      maxdim <- 0
    }
    maxit <- max(30,maxdim)
  }
  
  if(length(tol) == 0){
    tol <- 1e-10
  }
  
  tolb <- min(1e-4,tol*base::norm(b, type="2"))
  flag <- 1
  
  
  
  x <- matrix(0,N,1)
  if(base::norm(x,type="2")){
    #This won't happen apparently
  }else{
    r = b
  }
  err <- base::norm(r, type="2")
  resnrm <- err
  minresnrm <- err
  xx <- x
  
  omega <- 1
  r_tld <- r
  ##
  ##
  ##
  breakyes <- 0
  smtol <- 1e-40
  #resnrm <- c()
  for(iter in 1:maxit){
    rho <- as.numeric(t(r_tld) %*% r)
    if(abs(rho) < smtol){
      flag <- 2
      breakyes <- 1
      break
    }
    if(iter > 1){
      beta <- (rho/rho_1)*(alp/omega)
      p <- r + beta*(p - omega*v)
    }else{
      p <- r
    }
    p_hat <- precond(A,M1,p)
    if(is.list(A)){
      v <- matvec(A,p_hat)
    }else{
      v <- mexMatvec(A,p_hat)
    }
    alp <- as.numeric(rho/(t(r_tld) %*% v))
    s <- r - alp*v
    ##
    s_hat <- precond(A,M1,s)
    if(is.list(A)){
      t. <- matvec(A,s_hat)
    }else{
      t. <- mexMatvec(A,s_hat)
    }
    omega <- as.numeric((t(t.) %*% s)/(t(t.) %*% t.))
    x <- x + alp*p_hat + omega*s_hat
    r <- as.matrix(s) - omega*t.
    rho_1 <- rho
    ##
    ## check convergence
    ##
    err <- base::norm(r, type="2")
    resnrm <- c(resnrm,err)
    if(err < minresnrm){
      xx <- x
      minresnrm <- err
    }
    if(err < tolb){
      break
    }
    if(err > 10*minresnrm){
      breakyes <- 2
      print("^")
      break
    }
    if(abs(omega) < smtol){
      flag <- 2
      breakyes <- 1
      break
    }
  }
  return(list(xx=xx,resnrm=resnrm,flag=flag))
}
