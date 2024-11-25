symqmr <- function(A, b, L, tol=1e-10, maxit, printlevel=1){
  
  flag <- 0
  
  N <- length(b)
  
  if(length(maxit) == 0){
    if(is.null(A$mat22)){
      maxit <- 30
    }else{
      maxit <- max(30,max(dim(as.matrix(A$mat22))))
    }
  }
  
  if(length(tol) == 0){
    tol <- 1e-10
  }
  
  tolb <- min(1e-4, tol*base::norm(b,type="2"))
  solve_ok <- 1
  x <- matrix(0,N,1)
  if(base::norm(x,type="2") > 0){
    #This never occurs
  }else{
    r <- b
  }
  err <- base::norm(r, type="2")
  resnrm <- err
  minres <- err
  xx <- x
  if(err < 1e-3*tolb){
    return(list(xx=xx, resnrm=resnrm,solve_ok=solve_ok))
  }
  
  q <- precond(A,L,r)
  
  if(any(is.nan(q))){
    flag <- 1
    return(list(xx=xx, resnrm=resnrm,solve_ok=solve_ok,flag=flag))
  }
  
  tau_old <- base::norm(q,type="2")
  rho_old <- t(r) %*% q
  theta_old <- 0
  d <- matrix(0,N,1)
  res <- r
  Ad <- matrix(0,N,1)
  ##
  ##main loop
  ##
  tiny <- 1e-30
  for(iter in 1:maxit){
    if(is.list(A)){
      Aq <- matvec(A,q)
    }else{
      Aq <- A %*% q
    }
    sigma <- as.numeric(t(q) %*% as.matrix(Aq))
    if(abs(sigma) < tiny){
      solve_ok <- 2
      break
    }else{
      alpha <- as.matrix(rho_old/sigma)
      r <- r - c(alpha)*c(Aq)
    }
    u <- precond(A,L,r)
    if(any(is.nan(u))){
      flag <- 1
      return(list(xx=xx, resnrm=resnrm,solve_ok=solve_ok,flag=flag))
    }
    ##
    theta <- base::norm(u, type="2")/tau_old
    c. <- 1/sqrt(1 + theta^2)
    tau <- tau_old*theta*c.
    gam <- (c.^2*theta_old^2)
    eta <- c(c.^2*alpha)
    d <- gam*d + eta*q
    x <- x + d
    ##
    Ad <- gam*Ad + eta*Aq
    res <- res - Ad
    err <- base::norm(res, type="2")
    resnrm <- c(resnrm,err)
    if(err < minres){
      xx <- x
      minres <- err
    }
    if(err < tolb){
      break
    }
    if(iter > 10){
      if(err > 0.98*mean(resnrm[(iter-10):iter])){
        solve_ok <- 0.5
        break
      }
    }
    ##
    if(abs(as.numeric(rho_old)) < tiny){
      solve_ok <- 2
      break
    }else{
      rho <- t(r) %*% u
      beta <- as.numeric(rho/rho_old)
      q <- as.numeric(u) + beta*as.matrix(q)
    }
    rho_old <- rho
    tau_old <- tau
    theta_old <- theta
  }
  if(iter == maxit){
    solve_ok <- 0.3
  }
  return(list(xx=xx, resnrm=resnrm,solve_ok=solve_ok,flag=flag))
}