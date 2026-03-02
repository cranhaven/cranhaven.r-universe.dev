bbp <- function(w, k, a, b){
  if(a >= b){
    stop("[0, a] need to be disjoint from [b, 1].")
  } 
  l <- sum(w >= a & w <= b)
  v <- seq(a, b, length.out = l)
  vnew <- (v - a)/(b - a)
  basis <- array(0, dim = c(l, k+1))
  for(i in 0:k){
    basis[, i + 1] <- (choose(k, i)) * vnew^i * (1 - vnew)^(k - i)
  }
  return(list("basis" = basis, "angles" = v)) 
}

est_beta <- function(par, basis, t, len_vec, w, lam_end){ 
  if(any(lam_end > 1)){
    stop("Value of the ADF at the endpoints not supported.")
  }
  beta <- c(lam_end[1], exp(par), lam_end[2])
  lam <- basis %*% beta
  if(sum(is.infinite(beta)) > 0 | sum(is.na(beta)) > 0){
    return(1e10)
  }else{
    lam2 <- c() 
    for(i in 1:length(len_vec)){
      lam2[(length(lam2) + 1):(length(lam2) + len_vec[i])] <- rep(lam[i], len_vec[i])
    }
    loglike <- sum(log(lam2)) - sum(lam2 * t) 
    return(-loglike)
  }
}

minfunction_mle <- function(w, data, a, b, lam_end, k, q_minproj, tol, par_init){
  if(tol < 0){
    stop("Convergence tolerance needs to be positive.")
  }
  polynomials <- bbp(w = w, k = k, a = a, b = b)
  basis <- polynomials$basis
  angles <- polynomials$angles
  min_proj <- sapply(angles, function(i) minproj_lambda(data = data, w = i, q_minproj = q_minproj))
  t <- c()
  len_vec <- c()
  for(i in 1:length(angles)){
    aux <- min_proj[1, ][[i]][min_proj[1, ][[i]] > min_proj[2, ][[i]]]
    len_vec[i] <- length(aux)
    t[(length(t) + 1):(length(t) + len_vec[i])] <- aux - min_proj[2, ][[i]]
  }
  results <- tryCatch(optim(par = par_init, fn = est_beta, basis = basis, t = t, len_vec = len_vec, w = angles, lam_end = lam_end, method = "BFGS", control = list(maxit = 100000)),
                      error = function(e){1})
  if(is.list(results)){optim_output <- results}
  else{
    optim_output <- optim(par = par_init, fn = est_beta, basis = basis, t = t, len_vec = len_vec, w = angles, lam_end = lam_end, control = list(maxit = 100000))
  }
  results <- tryCatch(optim(par = optim_output$par, fn = est_beta, basis = basis, t = t, len_vec = len_vec, w = angles, lam_end = lam_end, control = list(maxit = 100000)),
                      error = function(e){1})
  if(is.list(results)){optim_output2 <- results}
  else{
    optim_output2 <- optim(par = optim_output$par, fn = est_beta, basis = basis, t = t, len_vec = len_vec, w = angles, lam_end = lam_end, control = list(maxit = 100000))
  }

  while(abs(optim_output2$val - optim_output$val) >= tol){
    optim_output <- optim_output2
    results <- tryCatch(optim(par = optim_output$par, fn = est_beta, basis = basis, t = t, len_vec = len_vec, w = angles, lam_end = lam_end, control = list(maxit = 100000)),
                        error = function(e){1})
    if(is.list(results)){optim_output2 <- results}
    else{
      optim_output2 <- optim(par = optim_output$par, fn = est_beta, basis = basis, t = t, len_vec = len_vec, w = angles, lam_end = lam_end, control = list(maxit = 100000))
    }
  }
  return(c(lam_end[1], exp(optim_output2$par), lam_end[2]))
}