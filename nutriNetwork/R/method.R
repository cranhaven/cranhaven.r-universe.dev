###################################
######### nutriNetwork ###########
###################################

cutoffs = function(y){
	p<-ncol(y)
	n<-nrow(y)
	k<-unique(sort(unlist(y)))
	n.levels<-length(k)
	q<-matrix(nrow=p,ncol=n.levels)
	for(i in 1:p){
		X=factor(y[,i],levels=k)
		No<-tabulate(X, nbins=n.levels)
		q[i,]<-qnorm(cumsum(No)/n)
	}
	q[ ,n.levels] <- Inf
	q<-cbind(-Inf,q)
	return(q)
}

lower.upper = function(y){
  cutoffs <- cutoffs(y)
  levels	<- unique(sort(unlist(y)))
  n.levels<- length(levels)
  n <- nrow(y)
  p <- ncol(y)
  lower = matrix(nrow=n,ncol=p)
  upper = matrix(nrow=n,ncol=p)
  for (i in 1:n){
    sel <- match(y[i,],levels)
    lower[i,]<-apply(cbind(sel,cutoffs),1,function(x){x[x[1]+1]})
    upper[i,]<-apply(cbind(sel,cutoffs),1,function(x){x[x[1]+2]})
  }
  lower[is.na(lower)] <- -Inf 
  upper[is.na(upper)] <- Inf	
  
  return(list(lower=lower,upper=upper))
}


#----------------------------------
calculate_EM_Gibbs = function(chain, y, rho=rho[1], Theta=NULL, lower.upper, em.tol=.001, em.iter=10, c.em.iter=1, gibbs.iter=1000, mc.iter=1000, ncores = 4)
{
  n = nrow(y)
  p = ncol(y) 
  c.em.iter = 1
  dif  =  100
  while((c.em.iter <= em.iter) && (dif >= em.tol)) 
  {   
    R   <- calculate.R.internal( y, theta = Theta, lower.upper = lower.upper, gibbs.iter = gibbs.iter, mc.iter = mc.iter, ncores = ncores)
    R.gl <- glasso(s= R, rho, maxit=1000, penalize.diagonal=FALSE)
    if(det(R.gl$w) <= 0)
    {
      R.gl$w <- nearPD(R.gl$w,keepDiag=TRUE)$mat 
    }
    dif <- sum(abs(Theta - R.gl$wi)/p^2) 
    Theta = as(R.gl$wi, "dgTMatrix") 
    Theta = as(Theta, "sparseMatrix") 
    c.em.iter <- c.em.iter + 1
  }
  
  results <- list()
  results$Theta	<- Matrix(Theta, sparse=TRUE)
  results$Sigma	<- Matrix(R.gl$w, sparse=TRUE)
  results$ES		<- Matrix(R, sparse = TRUE)
  results$Z		<- NULL
  results$rho		<- rho
  results$loglik	<- n/2 *(determinant(results$Theta, logarithm = TRUE)$modulus - sum(diag(results$ES %*%results$Theta)))
  
  return(results)
}


Gibbs_method = function(y, rho = NULL, n_rho = NULL, rho_ratio = NULL, Theta=NULL, ncores = 4, chain = 1, max.elongation = 10, em.tol=0.001) 
{
  p <- ncol(y)
  n <- nrow(y)
  lower.upper = lower.upper(y)
  
  if(is.null(rho))
  {
    if(is.null(n_rho)) n_rho = 10
    cr = cor(y, method="spearman") - diag(p)
    cr[is.na(cr)] <- 0
    rho_max = max(max(cr),-min(cr))
    if(rho_max == 0) 
    {
      ty <- npn(y, npn.func= "shrinkage")
      cr = cor(ty, method="spearman") - diag(p)
      rho_max = max(max(cr),-min(cr))
    }
    if(rho_max >= .7) rho_max = .7
    rho_min = rho_ratio * rho_max
    rho = exp(seq(log(rho_max), log(rho_min), length = n_rho))
    rm(cr, rho_max, rho_min, rho_ratio)
  }
  
  Gibbs.method <- calculate_EM_Gibbs(chain, y, rho = rho[chain], Theta=Theta, lower.upper = lower.upper, em.tol = em.tol, em.iter = max.elongation, gibbs.iter = 500, mc.iter = 1000, ncores = ncores)
  invisible(return(Gibbs.method))
}  

calcRcore <- function(i, n, p, gibbs.iter, mc.iter, theta, lower.upper, verbose = TRUE){
  z.gibbs <- rtmvnorm.sparseMatrix(n=mc.iter, H= theta , lower=lower.upper$lower[i,], upper=lower.upper$upper[i,], burn.in=gibbs.iter)
  summed <- matrix(0, p, p)
  for(iteration in 1: mc.iter)
  { 
    summed <- summed + (z.gibbs[iteration,] %*% t(z.gibbs[iteration,]))
  }
  R <- summed /  mc.iter
  return(R)
}

calculate.R.internal = function(y, theta=NULL, lower.upper=NULL, gibbs.iter=1000, mc.iter=1000, ncores = 4, verbose = TRUE)
{
  if(missing(y)) stop("argument \"y\" is missing, with no default")
  S <- 0
  p <- ncol(y)
  n <- nrow(y)
  if(missing(lower.upper)) lower.upper <- lower.upper(y)
  if(ncores > 1){
    cl <- makeCluster(ncores)
    scovs <- parLapply(cl = cl, 1:n, function(i) { 
      calcRcore(i, n, p, gibbs.iter, mc.iter, theta, lower.upper, verbose); 
    })
    stopCluster(cl)
  }else{
    scovs <- lapply(1:n, function(i){ calcRcore(i, n, p, gibbs.iter, mc.iter, theta, lower.upper); })
  }
  
  for(i in 1:n){ 
    S <- S + scovs[[i]]
  }
  ES  <- cov2cor(S/n)
  rm(S)
  return(ES)
}

R.gibbs = function(y, theta, gibbs.iter=1000, mc.iter=500, ncores=NULL, verbose = TRUE)
{
  if(is.null(ncores)) ncores= detectCores() - 1
  if(missing(theta)) 
  {
    theta <- sparseMatrix(i = 1:ncol(y), j = 1:ncol(y), x = 1)
  }else{
    theta <- as(theta, "dgTMatrix") 
    theta <- as(theta, "sparseMatrix") 
  }
  lower.upper <- lower.upper(y)
  ES <- calculate.R.internal(y, theta, lower.upper, gibbs.iter, mc.iter, ncores, verbose)
  return(ES = ES)
}

#----------------------------------
npn = function(x, npn.func = "shrinkage"){
  gcinfo(FALSE)
  n = nrow(x)
  p = ncol(x)
  x.col = colnames(x)
  x.row = rownames(x)
  
  if(npn.func == "shrinkage"){
    
    x = qnorm(apply(x,2,rank)/(n+1))
    x = x/sd(x[,1])
    colnames(x) = x.col
    rownames(x) = x.row
  }
  
  if(npn.func == "skeptic"){
    x = 2*sin(pi/6*cor(x,method="spearman"))
    colnames(x) = x.col
    rownames(x) = x.col
  }
  return(x)
}
#----------------------------------

calculate_EM_approx = function(chain, y, Z, rho, Sigma, Theta, lower_upper, em_tol, em_iter, ncores=4, verbose=FALSE )
{
  c_em_iter = 1
  dif	<- 100
  p 	<- ncol(y)
  n	<- nrow(y)
  s <- proc.time()	
  while((c_em_iter < em_iter) && (dif >= em_tol ))
  {
    S_obj	<- calculate.R.approx.internal(y=y, Z=Z, lower_upper=lower_upper, Sigma= Sigma, ncores=ncores)
    Z		<- S_obj$Z
    S_gl	<- glasso(s=S_obj$ES, rho=rho, maxit=1000, penalize.diagonal=FALSE)	
    Theta <- (t(S_gl$wi) + S_gl$wi) / 2
    Sigma_new <- (t(S_gl$w) + S_gl$w) / 2
    sd_marginal <- sqrt(diag(Sigma_new))
    sd_marginal[abs(sd_marginal) < 1e-10] <- 1e-10			
    Sigma_new	<- diag(1/sd_marginal) %*% Sigma_new %*% diag(1/sd_marginal)
    Theta	<- Matrix(diag(sd_marginal) %*% Theta %*% diag(sd_marginal))	
    dif		<-	sum(abs(Sigma_new - Sigma)/p^2) 
    Sigma	<-  Sigma_new
    c_em_iter <- c_em_iter + 1
    rm(Sigma_new, sd_marginal, S_gl)
    gc()
  }
  
  results <- list()
  results$Theta	<- Theta
  results$Sigma	<- Sigma
  results$ES		<- S_obj$ES
  results$Z		<- Z
  results$rho		<- rho
  results$loglik	<- n/2 *(determinant(results$Theta, logarithm = TRUE)$modulus - sum(diag(results$ES %*%results$Theta)))
  
  return(results)
}


element_S_j = function(j, lower_upper, mu=0, sigma=1)
{
  delta1 <- (lower_upper$lower[ ,j] - mu) / sigma
  delta2 <- (lower_upper$upper[ ,j] - mu) / sigma
  tmp1 <- (dnorm(delta1) - dnorm(delta2)) / (pnorm(delta2) - pnorm(delta1))
  EX <- mu + tmp1 * sigma
  
  delta1[delta1 < -1e+10] <- -1e+10
  delta2[delta2 > 1e+10] <- 1e+10
  tmp2 <- (delta1*dnorm(delta1) - delta2*dnorm(delta2)) / (pnorm(delta2) - pnorm(delta1))
  EXX <- sigma^2 + mu^2 + sigma^2 * tmp2 + 2 * mu * sigma * tmp1
  rm(delta1, delta2, tmp1, tmp2)
  gc()
  
  return(list(EX=EX, EXX=EXX))
}

approx_method = function(y, Z, ES=NULL, rho = NULL, lower_upper=NULL, chain=1, ncores=4, em_tol=.001, em_iter=10 )
{
  obj <- glasso(s=ES, rho=rho[chain], maxit=1000, penalize.diagonal=FALSE)
  Theta <- Matrix((t(obj$wi) + obj$wi) / 2, sparse = TRUE)
  Sigma <- (t(obj$w) + obj$w) / 2
  sd_marginal  <- sqrt(diag(Sigma))
  sd_marginal[abs(sd_marginal) < 1e-10] <- 1e-10
  Sigma <- diag(1/sd_marginal) %*% Sigma %*% diag(1/sd_marginal)
  Theta <- diag(sd_marginal) %*% Theta %*% diag(sd_marginal)
  rm(obj, sd_marginal)
  gc()
  
  approx.method <- calculate_EM_approx(chain=chain, y=y, Z=Z, rho= rho[chain], Sigma=Sigma, Theta= Theta, lower_upper=lower_upper, em_tol=em_tol, em_iter = em_iter, ncores=ncores)
  
  invisible(return(approx.method))
}

initialize = function(y, rho = NULL, n_rho = NULL, rho_ratio = NULL, ncores=NULL )
{
  p <- ncol(y)
  n <- nrow(y)
  lower_upper = lower.upper(y)
  
  if(is.null(rho))
  {
    if(is.null(n_rho)) n_rho = 10
    if(is.null(rho_ratio)) rho_ratio = 0.3
    cr = cor(y, method="spearman") - diag(p)
    cr[is.na(cr)] <- 0
    rho_max = max( max(cr),-min(cr) )
    if(rho_max == 0) 
    {
      ty <- npn(y, npn.func= "shrinkage")
      cr = cor(ty, method="spearman") - diag(p)
      rho_max = max(max(cr),-min(cr))
    }
    if(rho_max >= .7) rho_max = .7
    rho_min = rho_ratio * rho_max
    rho = exp(seq(log(rho_max), log(rho_min), length = n_rho))
    rm(cr, rho_max, rho_min, rho_ratio)
    gc()
  }
  
  Z <- matrix(0, n, p)
  diag_element <- rep(0, p)
  if(ncores > 1)
  {
    cl <- makeCluster(ncores)
    tmp2 <- parLapply(cl = cl, 1:p, function(i) { 
      element_S_j(i, lower_upper ); 
    })
    stopCluster(cl)
  }else{
    tmp2 <- lapply(1:p, function(i){ element_S_j(i, lower_upper );})
  }
  Z <-  do.call(cbind, lapply(1:p, function(x) tmp2[[x]]$EX ))
  diag_element <- unlist(lapply(1:p, function(x)mean(tmp2[[x]]$EXX)))
  ES <- t(Z) %*% Z / n
  diag(ES) <- diag_element
  rm(tmp2, diag_element)	 
  gc() 
  
  return(list(Z=Z, ES = ES, rho = rho, lower_upper=lower_upper))
}	

cond_N <- function(j, Sigma, Z , Z_new, diag_element, lower_upper)
{
  p <- ncol(Sigma)
  tmp <- matrix(Sigma[j, -j], 1, p-1)
  tmp1 <- solve(Sigma[-j, -j])
  
  mu <- tmp %*% tmp1 %*% t(Z[, -j])				
  mu <- as.vector(mu)		 
  sigma <- Sigma[j, j] - tmp %*% tmp1 %*% t(tmp)
  sigma <- sqrt(sigma)  		
  
  obj <- element_S( lower= lower_upper$lower[ ,j], upper= lower_upper$upper[ ,j], mu=mu, sigma=sigma)      
  
  Z_new <- obj$EX
  diag_element <- mean(obj$EXX)
  
  rm(tmp, tmp1, mu, sigma, obj )
  gc()
  
  return(list(Z_new=Z_new, diag_element=diag_element))
}

element_S <- function( lower, upper, mu=0, sigma=1)
{
  delta1 <- (lower - mu) / sigma
  delta2 <- (upper - mu) / sigma
  tmp1 <- (dnorm(delta1) - dnorm(delta2)) / (pnorm(delta2) - pnorm(delta1))
  EX <- mu + tmp1 * sigma
  
  delta1[delta1 < -1e+10] <- -1e+10
  delta2[delta2 > 1e+10] <- 1e+10
  tmp2 <- (delta1*dnorm(delta1) - delta2*dnorm(delta2)) / (pnorm(delta2) - pnorm(delta1))
  EXX <- sigma^2 + mu^2 + sigma^2 * tmp2 + 2*mu*sigma*tmp1
  rm(delta1, delta2, tmp1, tmp2 )
  gc()
  
  return(list(EX=EX, EXX=EXX))
}

element_S <- function( lower, upper, mu=0, sigma=1)
{
  delta1 <- (lower - mu) / sigma
  delta2 <- (upper - mu) / sigma
  tmp1 <- (dnorm(delta1) - dnorm(delta2)) / (pnorm(delta2) - pnorm(delta1))
  EX <- mu + tmp1 * sigma
  
  delta1[delta1 < -1e+10] <- -1e+10
  delta2[delta2 > 1e+10] <- 1e+10
  tmp2 <- (delta1*dnorm(delta1) - delta2*dnorm(delta2)) / (pnorm(delta2) - pnorm(delta1))
  EXX <- sigma^2 + mu^2 + sigma^2 * tmp2 + 2*mu*sigma*tmp1
  rm(delta1, delta2, tmp1, tmp2 )
  gc()
  
  return(list(EX=EX, EXX=EXX))
}

cond_N <- function(j, Sigma, Z , Z_new, diag_element, lower_upper)
{
  p <- ncol(Sigma)
  tmp <- matrix(Sigma[j, -j], 1, p-1)
  tmp1 <- solve(Sigma[-j, -j])
  
  mu <- tmp %*% tmp1 %*% t(Z[, -j])				
  mu <- as.vector(mu)		 
  sigma <- Sigma[j, j] - tmp %*% tmp1 %*% t(tmp)
  sigma <- sqrt(sigma)  		
  
  obj <- element_S( lower= lower_upper$lower[ ,j], upper= lower_upper$upper[ ,j], mu=mu, sigma=sigma)      
  
  Z_new <- obj$EX
  diag_element <- mean(obj$EXX)
  
  rm(tmp, tmp1, mu, sigma, obj )
  gc()
  
  return(list(Z_new=Z_new, diag_element=diag_element))
}

calculate.R.approx.internal <- function(y, Z, lower_upper, Sigma, ncores = NULL )
{      
  if(is.null(ncores)) ncores = detectCores() - 1
  p <- ncol(y)
  n <- nrow(y) 
  Z_new <- rep(0, n)
  
  if(ncores > 1){
    cl <- makeCluster(ncores)
    Sigma <- Sigma
    Z <- Z
    cond_norm <- parLapply(cl = cl, 1:p, function(j) { 
      cond_N(j, Sigma=Sigma, Z=Z , Z_new=Z_new, diag_element=diag_element, lower_upper=lower_upper ); 
    })
    stopCluster(cl)
  }else{
    cond_norm <- lapply(1:p, function(j){ cond_N(j, Sigma, Z , Z_new, diag_element, lower_upper); } )
  }
  
  Z_new <- do.call(cbind, lapply(1:p, function(x) cond_norm[[x]]$Z_new ))
  diag_element <- sapply(1:p, function(x) cond_norm[[x]]$diag_element)
  
  ES <- t(Z_new) %*% Z_new / n
  diag(ES) <- diag_element
  
  output <- list()
  output$ES <- ES
  output$Z <- Z_new
  
  rm(lower_upper, ES, Z_new )
  return(output)         
}

R.approx = function(y, Z = NULL, Sigma=NULL, rho = NULL ,  ncores = NULL )
{
  p <- ncol(y)
  n <- nrow(y) 
  if(is.null(ncores)) ncores= detectCores()
  lower.upper <- lower.upper(y)
  
  if( is.null(Z) )
  {
    Z <- matrix(0, n, p)
    diag_element <- rep(0, p)
    if(ncores > 1)
    {
      cl <- makeCluster(ncores)
      tmp2 <- parLapply(cl = cl, 1:p, function(i) { 
        element_S_j(i, lower.upper ); 
      })
      stopCluster(cl)
    }else{
      tmp2 <- lapply(1:p, function(i){ element_S_j(i, lower.upper );})
    }
    Z <-  do.call(cbind, lapply(1:p, function(x) tmp2[[x]]$EX ))
    diag_element <- unlist(lapply(1:p, function(x)mean(tmp2[[x]]$EXX)))
    ES <- t(Z) %*% Z / n
    diag(ES) <- diag_element
    rm(tmp2, diag_element)	 
    gc() 
  }
  
  if(is.null(Sigma))
  {
    if(is.null(rho)) rho = max(max(ES),-min(ES))
    obj <- glasso(s=ES, rho=rho, maxit=1000, penalize.diagonal=FALSE)
    Sigma <- (t(obj$w) + obj$w) / 2
    sd_marginal  <- sqrt(diag(Sigma))
    sd_marginal[ abs(sd_marginal) < 1e-10 ] <- 1e-10
    Sigma <- diag(1/sd_marginal) %*% Sigma %*% diag(1/sd_marginal)
  }
  
  calculate.R.approx.internal( y, Z, lower.upper, Sigma, ncores = ncores)
}

#----------------------------------
isInf <- function(x) x > 0 & is.infinite(x)

rmvnorm = function(n, mu=NULL, sigma )
{
  if(is.null(mu)) mu = rep(0, n)
  p = nrow(sigma)
  y  <- matrix(rnorm(n*p),nrow = p, ncol= n)
  chl<- chol(sigma)
  z <- t( t(chl) %*% y + mu )
  return(z)
}

dmvnorm <- function (x, mean = rep(0, p), sigma = diag(p), log = FALSE)
{
  p = ncol(x)
  if(is.null(mean)) mean = rep(0, p)
  if (is.vector(x))
    x <- matrix(x, ncol = length(x))
  p <- ncol(x)
  if(!missing(mean)) {
    if(!is.null(dim(mean))) dim(mean) <- NULL
    if (length(mean) != p)
      stop("mean and sigma have non-conforming size")
  }
  if(!missing(sigma)) {
    if (p != ncol(sigma))
      stop("x and sigma have non-conforming size")
    if (!isSymmetric(sigma, tol = sqrt(.Machine$double.eps),
                     check.attributes = FALSE))
      stop("sigma must be a symmetric matrix")
  }
  
  
  dec <- tryCatch(chol(sigma), error=function(e)e)
  if (inherits(dec, "error")) {
    x.is.mu <- colSums(t(x) != mean) == 0
    logretval <- rep.int(-Inf, nrow(x))
    logretval[x.is.mu] <- Inf 
  } else {
    tmp <- backsolve(dec, t(x) - mean, transpose = TRUE)
    rss <- colSums(tmp ^ 2)
    logretval <- - sum(log(diag(dec))) - 0.5 * p * log(2 * pi) - 0.5 * rss
  }
  names(logretval) <- rownames(x)
  if(log) logretval else exp(logretval)
}

rmvt <- function(n, sigma = diag(2), df = 1,
                 delta = rep(0, nrow(sigma)),
                 type = c("shifted", "Kshirsagar"))
{
  if (length(delta) != nrow(sigma))
    stop("delta and sigma have non-conforming size")
  
  if (df == 0 || isInf(df))
  {
    return(rmvnorm(n, mu = delta, sigma = sigma))
  }	
  type <- match.arg(type)
  switch(type,
         "Kshirsagar" = {
           return(rmvnorm(n, mu = delta, sigma = sigma)/
                    sqrt(rchisq(n, df)/df))
         },
         "shifted" = {
           sims <- rmvnorm(n, sigma = sigma)/sqrt(rchisq(n, df)/df)
           return(sweep(sims, 2, delta, "+"))
         },
         stop("wrong 'type'"))
}
