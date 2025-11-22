kurtosis <- function(Y,mu,B,D,tau){
  n <- nrow(Y); p <- ncol(Y); g <- dim(B)[3]; 
  kurt <- matrix(0,ncol = g, nrow = 1)
  for(i in 1:g){
    b.2p <- sum(tau[,i]*mahalanobis(Y, mu[,i], B[,,i]%*%t(B[,,i])+D[,,i])^2)/sum(tau[,i])
    kurt[1,i] <- (b.2p - p*(p+2))/sqrt(8*p*(p+2)/sum(tau[,i]))
  }
  return(kurt)
}

progress <- function (bestmodel) {
 g <-dim(bestmodel$model$B)[3]
 bic <- bestmodel$diagnostics$bic
 cat(sprintf('\rThe current number of components is %d and the BIC is %f',
                  g,
                  bic))
 
}

MFA.ecm.withInit <- function(Y, g = 2, q, q_update = FALSE,eta = 0.5e-2 ,itmax = 500, tol = 1e-05,
                             init_para, conv_measure = "diff", 
                             warn_messages = TRUE,
                             ...){
  #init_para is of the form list(g = g, q = q, pivec = pivec, B = B, mu = mu, 
  #D = D, sigma_type = sigma_type, D_type = D_type)
  
  start.time <- Sys.time()
  n <- nrow(Y); p <- ncol(Y); 
  warn_msg <- NULL
  init_clust <- NULL;
  
  model <- NULL
  
  estd_model <- est.mfa.ecm(init_para = init_para,
                            Y = Y, g = g, itmax = itmax, tol = tol,
                            conv_measure = conv_measure, eta = eta, q_update)
  
  if(class(estd_model) == "warn" || class(estd_model) == "error") {
    when <- paste("Couldn't estimate model")
    what <- estd_model
    warn_msg <- rbind(warn_msg, cbind(when, what))
    return(warn_msg)
  }
  
  end.time <- Sys.time()
  out <- vector(mode = 'list')
  
  p <- ncol(Y); g <- estd_model$g; q <- estd_model$q
  d <- (g - 1) + 2*g*p + g*(p*q - q*(q-1)/2)
  bic <- log(n)*d-2*estd_model$logL;
  
  model$numFactors <- matrix(rep(estd_model$q,estd_model$g),nrow = 1)
  out$model <- estd_model[-c(1,2,7,8,9,10)]
  out$diagnostics$logL <- estd_model$logL
  out$diagnostics$bic <- bic
  out$diagnostics$totalTime <- as.numeric(difftime(end.time, start.time, units = 'secs'))
  out$diagnostics$warnings <- warn_msg
  out$clustering$responsibilities <- estd_model$tau
  out$clustering$allocations <- matrix(Rfast::rowMaxs(estd_model$tau),nrow = 1)
  return(out)
  
}

dobirth.amfa <- function(Y,mu,B,D,tau,parent,hardness = 1){
  #Hardness : 1 means hard, 0.5 means soft, 0 is equiv to 1 (opposite direction)
  
  # dobirth.m : This script performs a birth operation. It has
  # considerable flexibility in the way it does this, along with a few
  # tweakables, but here the operation is to split the
  # *responsibilities* of the parent component.
  # original code by M.Beal.
  
  n = dim(Y)[1];
  p = dim(Y)[2]
  g <- dim(B)[3]
  
  t = g + 1; # t is the identity of the newborn.
  
  # Sample from the full covariance ellipsoid of the component
  delta_vector = matrix(MASS::mvrnorm(1,matrix(0,nrow = 1, ncol = p),B[,,parent]%*%t(B[,,parent]) + D[,,parent] ),ncol = 1);
  #delta_vector = matrix((1/p)*(1:p),ncol = 1) #In case of wanting deterministic testing
  # tau birth
  assign = sign( sweep(Y, 2, mu[,parent], '-') %*% delta_vector ); # size n x 1
  
  pos_ind = matrix(assign == 1,ncol = 1);
  neg_ind = matrix(assign == -1, ncol = 1);
  # Reassign those one side of vector to the child, t,
  # whilst the rest remain untouched. Positive are sent to child
  tau = cbind(tau, matrix(tau[,1]))
  tau[pos_ind,t] = hardness*tau[pos_ind,parent];
  tau[pos_ind,parent] = (1-hardness)*tau[t(pos_ind),parent];
  tau[neg_ind,t] = (1-hardness)*tau[t(neg_ind),parent];
  tau[neg_ind,parent] = hardness*tau[t(neg_ind),parent];
  # set all features of t to those of t_parent
  B <- abind::abind(B,matrix(B[,,parent],nrow = p),along= 3)
  mu <- abind::abind(mu,matrix(mu[,parent],nrow = p)+ delta_vector,along= 2)
  D <- abind::abind(D,matrix(D[,,parent],nrow = p),along= 3)
  mu[,parent] <- mu[,parent] - delta_vector
  
  return(list(mu = mu,B = B,D = D,tau = tau))
  
}


