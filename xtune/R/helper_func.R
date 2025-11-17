##------------ xtune.fit utility functions
approx_likelihood.xtune <- function(to_estimate, input_X, input_Y, input_Z, sigma.square.est, input_c) {
  X = input_X
  Y = input_Y
  Z = input_Z
  c = input_c
  sigma_square = sigma.square.est
  n = nrow(X)
  lambda = exp(Z %*% to_estimate)
  gamma = 2/(lambda^2*c^2 + 2*lambda*(1-c))
  K = sigma_square * diag(n) + X %*% diag(c(gamma)) %*% t(X)
  logdetK = determinant(K)$modulus[1]
  part1 = t(Y) %*% solve(K, Y)
  normapprox = 1/2 * (part1 + logdetK)
  return(as.numeric(normapprox))
}

update_alpha.xtune <- function(X, Y, Z,c,alpha.old, alpha.max, epsilon, sigma.square, theta, maxstep_inner,
                            margin_inner, verbosity) {
  ## initial
  alpha.inner.old = alpha.old
  k_inner = 1
  n = nrow(X)
  p = ncol(X)
  while (k_inner < maxstep_inner) {
    # given alpha update delta
    lambda = exp(Z %*% alpha.inner.old)
    gamma = 2/(lambda^2*c^2 + 2*lambda*(1-c))

    sd_y <- sqrt(var(Y) * (n - 1)/n)
    C = sum(1/gamma)/p * sd_y * sigma.square/n
    delta.est = coef(glmnet(X, Y, alpha = 0, penalty.factor = 1/gamma, lambda = C,
                            standardize = F, intercept = FALSE))[-1]

    ## given delta update alpha
    alpha.inner.new <- optim(alpha.old, likelihood.alpha.theta.xtune,likelihood.alpha.theta.gradient.xtune,
                             c =c,Z = Z, theta = theta, delta = delta.est,method = "L-BFGS-B", upper = c(alpha.max*epsilon, rep(Inf, length(alpha.old)-1)))$par
    if (sum(abs(alpha.inner.new - alpha.inner.old)) < margin_inner) {
      break
    }

    if (verbosity == TRUE) {
      cat(blue$italic("#-----------------"),magenta("Inner loop Iteration",k_inner,"Done"),blue$italic("-----------------#\n"),sep = "")
    }
    k_inner = k_inner + 1
    alpha.inner.old <- alpha.inner.new
  }
  return(list(alpha.est = alpha.inner.old, inner_iter = k_inner))
}

likelihood.alpha.theta.xtune <- function(Z,c, alpha, theta, delta) {
  lambda = exp(Z %*% alpha)
  gamma = 2/(lambda^2*c^2 + 2*lambda*(1-c))

  return(as.numeric(t(theta) %*% gamma + delta^2 %*% (1/gamma)))
}


likelihood.alpha.theta.gradient.xtune <- function(Z,c, alpha, theta, delta) {
  lambda = exp(Z %*% alpha)
  gamma = 2/(lambda^2*c^2 + 2*lambda*(1-c))

  dev_gamma = theta - delta^2/(gamma^2)
  dev_gamma_alpha = as.vector((-2*(2*(1-c) + 2*c^2*lambda))/(2*lambda*(1-c) + (c*lambda)^2)^2 *lambda) *Z
  return(crossprod(dev_gamma, dev_gamma_alpha))
}

score_function <- function(to_estimate, input_X, input_Y, input_Z, sigma.square.est, input_c) {
  X = input_X
  Y = input_Y
  Z = input_Z
  c = input_c
  sigma_square = sigma.square.est
  n = nrow(X)
  p = ncol(X)
  lambda = exp(Z %*% to_estimate)
  gamma = 2/(lambda^2*c^2 + 2*lambda*(1-c))
  Rinv <- backsolve(chol(crossprod(X)/sigma_square + diag(c(1/gamma))), diag(1,
                                                                             p))
  diagSigma <- rowSums(Rinv^2)
  mu_vec <- (Rinv %*% (crossprod(Rinv, crossprod(X, Y))))/sigma_square
  dev_gamma = (gamma - diagSigma - mu_vec^2)/(gamma^2)
  return(-crossprod(dev_gamma, as.vector(gamma) * Z))
}

relative_change<-function(a,b){
  return((a-b)/b)
}

##------------ mxtune.fit utility functions
estimate.alpha.mxtune <- function(X,Y,Z,c,B_inv,
                           alpha.init, alpha.max,
                           epsilon,
                           maxstep,
                           margin,
                           maxstep_inner,
                           margin_inner,
                           compute.likelihood,
                           verbosity){

  n = nrow(X);p=ncol(X);k = ncol(B_inv)

  ## Initialize
  alpha.old = alpha.init
  itr = 1

  while(itr < maxstep){
    # Given alpha, update theta
    lambda = exp(Z%*%alpha.old)
    gamma = 2/(lambda^2*c^2 + 2*lambda*(1-c))
    Sigma_y = apply(B_inv, 2 , function(g) list(diag(g) + (t(t(X)*c(gamma)))%*%t(X)))
    theta = t(sapply(Sigma_y, function(g) colSums(X*solve(g[[1]],X))))


    # Given theta, update alpha
    update.result <-update_alpha.mxtune(X,Y,Z,c = c,B_inv = B_inv, alpha.old = alpha.old,alpha.max, theta = theta, epsilon = epsilon, maxstep_inner = maxstep_inner,margin_inner = margin_inner)
    alpha.new <- update.result$alpha.est

    # Check convergence
    if(sum(abs(alpha.new - alpha.old)) < margin ){
      break
    }
    alpha.old <- alpha.new

    if (verbosity == TRUE) {
      cat(blue$italic("#-----------------"),magenta("Inner loop Iteration",itr,"Done"),blue$italic("-----------------#\n"),sep = "")
    }
    itr <- itr+1

  }
  return(alpha.new)
}

update_alpha.mxtune<-function(X,Y,Z,c,B_inv,alpha.old,alpha.max, theta, epsilon, maxstep_inner,margin_inner){

  ## initial
  alpha.iner.old = alpha.old
  i_inner = 1
  n=nrow(X)
  p=ncol(X)
  k = ncol(B_inv)
  B = 1/B_inv
  while (i_inner < maxstep_inner){
    # given alpha update delta
    lambda = exp(Z%*%alpha.iner.old)
    gamma = 2/(lambda^2*c^2 + 2*lambda*(1-c))

    X_B_sqrt = NULL; Y_B_sqrt = NULL; sd_y = NULL; C = NULL;delta.est = NULL
    for (i in 1:k) {
      X_B_sqrt[[i]] = X*sqrt(B[,i])
      Y_B_sqrt[[i]] = Y[,i]*sqrt(B[,i])
      sd_y[[i]] <- sqrt(var(Y_B_sqrt[[i]])*(n-1)/n)
      C[[i]]=sum(1/gamma)/p* sd_y[[i]]*1/n

      delta.est[[i]]=coef(glmnet(X_B_sqrt[[i]],Y_B_sqrt[[i]],alpha=0, penalty.factor = 1/gamma,lambda = C[[i]], standardize = F, intercept = FALSE))[-1]
    }

    ## given delta update alpha with constrain
    alpha.iner.new <- optim(alpha.old,likelihood.alpha.theta.mxtune,likelihood.alpha.theta.gradient.mxtune,c =c,Z=Z,theta = theta,delta=delta.est,
                            k = k,method = "L-BFGS-B", upper = c(alpha.max*epsilon, rep(Inf, length(alpha.old)-1)))$par
    if (sum(abs(alpha.iner.new - alpha.iner.old)) < margin_inner){
      break
    }
    i_inner = i_inner + 1
    alpha.iner.old <- alpha.iner.new
  }
  return(list(alpha.est=alpha.iner.old,inner_iter = i_inner))
}

likelihood.alpha.theta.mxtune<-function(Z,c,alpha,theta,delta,k){
  lambda = exp(Z %*% alpha)
  gamma = 2/(lambda^2*c^2 + 2*lambda*(1-c))

  L.theta.alpha = NULL
  for (i in 1:k) {
    L.theta.alpha[i] = t(theta[i,])%*%gamma + delta[[i]]^2%*%(1/gamma)
  }

  return(as.numeric(sum(L.theta.alpha)))
}

likelihood.alpha.theta.gradient.mxtune<-function(Z,c,alpha,theta,delta,k){

  lambda = exp(Z %*% alpha)
  gamma = 2/(lambda^2*c^2 + 2*lambda*(1-c))

  dev_gamma_alpha = as.vector((-2*(2*(1-c) + 2*c^2*lambda))/(2*lambda*(1-c) + (c*lambda)^2)^2 *lambda) *Z
  dev_gamma = NULL;L.prime = NULL
  for (i in 1:k) {
    dev_gamma[[i]] = theta[i,] - delta[[i]]^2/(gamma^2)
  }
  L.prime = t(sapply(dev_gamma, function(g) crossprod(g, dev_gamma_alpha)))

  return(colSums(L.prime))
}

approx_likelihood.mxtune <- function(B_inv,X,Y,V_inv,k,n) {
  normapprox = NULL
  for (i in 1:k) {
    K = B_inv[,i] + X %*% V_inv %*% t(X)
    logdetK = determinant(K)$modulus[1]
    part1 = t(as.numeric(Y[,i])) %*% solve(K, Y[,i])
    normapprox[i] = part1 + logdetK
  }

  return(-0.5*n*log(2*pi) + sum(as.numeric(normapprox)))
}
