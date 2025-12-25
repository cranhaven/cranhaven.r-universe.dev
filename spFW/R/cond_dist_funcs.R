####################################################################################
## Full conditional distributions
####################################################################################

######## no spatial ########

## posterior for g
q_g_FW_I <- function(Y, VAR_ind, ENV_ind, h, b, sig_e2, sig_g2, D, geno_ind){
  b1 <- b[VAR_ind]
  h1 <- h[ENV_ind]
  y_star <- Y - h1 - b1 * h1
  b_g <- (1/sig_e2) * unlist(lapply(X = geno_ind, FUN = function(x){
    return( sum(y_star[x]) )
  }))

  Q_g <- (1/sig_e2) * D + (1/sig_g2)
  z_g <- rnorm( length(b_g), 0, 1 )
  g_out <-  Q_g^(-1/2) * z_g + Q_g^(-1) * b_g

  return(g_out)
}


q_g_FW_A <- function(Y, VAR_ind, ENV_ind, h, b, sig_e2, sig_g2, D, M, La, DM, geno_ind){
  b1 <- b[VAR_ind]
  h1 <- h[ENV_ind]
  y_star <- Y - h1 - b1 * h1
  b_g <- unlist(lapply(X = geno_ind, FUN = function(x){
    return( sum(y_star[x]) )
  }))

  rho <-  sig_e2/ sig_g2
  La1 <- 1 + rho * La
  z_g <- rnorm( length(b_g), 0, 1 )
  v_g <- sqrt(sig_e2) * arma_Xy( X = DM, y = z_g / sqrt(La1) )
  mu_g <- arma_Xy( X = DM, y = (1/La1) * arma_Xty(X = DM, y =b_g) )
  g_out <- v_g + mu_g

  return(g_out)
}



## psterior for b
q_b_FW_I <- function(Y, VAR_ind, ENV_ind, g, h, sig_e2, sig_b2, geno_ind){
  g1 <- g[VAR_ind]
  h1 <- h[ENV_ind]
  y_star <- (Y - g1 - h1 ) * h1

  b_b <- (1/sig_e2) * unlist(lapply(X = geno_ind, FUN = function(x){
    return( sum(y_star[x]) )
  }))

  diag_b <- unlist(lapply(X = geno_ind, FUN = function(x){
    return( sum( (h1[x])^2 ) )
  }))

  Q_b <- (1/sig_e2) * diag_b + (1/sig_b2)
  z_b <- rnorm( length(b_b), 0, 1 )
  b_out <-  Q_b^(-1/2) * z_b + Q_b^(-1) * b_b

  return(b_out)
}



q_b_FW_A <- function(Y, VAR_ind, ENV_ind, g, h, sig_e2, sig_b2, A1_inv, geno_ind){
  g1 <- g[VAR_ind]
  h1 <- h[ENV_ind]
  y_star <- (Y - g1 - h1 ) * h1

  b_b <- (1/sig_e2) * unlist(lapply(X = geno_ind, FUN = function(x){
    return( sum(y_star[x]) )
  }))

  diag_b <- unlist(lapply(X = geno_ind, FUN = function(x){
    return( sum( (h1[x])^2 ) )
  }))

  Q_b <- (1/sig_e2) * diag(diag_b) + (1/sig_b2) * A1_inv
  L_b <- arma_chol( Q_b )
  w_b <- forwardsolve(l=L_b, x=b_b)
  mu_b <- forwardsolve(l=L_b, x=w_b, transpose = TRUE)
  z_b <- rnorm( length(b_b), 0, 1 )
  v_b <- forwardsolve(l=L_b, x=z_b, transpose = TRUE)
  b_out <- v_b + mu_b

  return(b_out)
}





## posterior for h
q_h_FW <- function(Y, VAR_ind, g, b, sig_e2, sig_h2, loc_rela){
  g1 <- g[VAR_ind]
  b1 <- b[VAR_ind]
  y_star <- (1 + b1) * (Y - g1 )

  b_h <- (1/sig_e2) * unlist(lapply(X = loc_rela$loc_ind, FUN = function(x){
    return( sum(y_star[x]) )
  }))

  diag_h <- unlist(lapply(X = loc_rela$loc_ind, FUN = function(x){
    return( sum( (1 + b1[x])^2 ) )
  }))

  Q_h <- (1/sig_e2) * diag_h + (1/sig_h2)
  z_h <- rnorm(length(b_h),0,1)
  h_out <-  Q_h^(-1/2) * z_h + Q_h^(-1) * b_h

  return(h_out)
}


q_h_FW_Z <- function(Y, VAR_ind, Z, g, b, ga, sig_e2, sig_h2, loc_rela){
  g1 <- g[VAR_ind]
  b1 <- b[VAR_ind]
  y_star <- (1 + b1) * (Y - g1 )

  b_h0 <- (1/sig_e2) * unlist(lapply(X = loc_rela$loc_ind, FUN = function(x){
    return( sum(y_star[x]) )
  }))

  diag_h <- unlist(lapply(X = loc_rela$loc_ind, FUN = function(x){
    return( sum( (1 + b1[x])^2 ) )
  }))

  Q_h <- (1/sig_e2) * diag_h + (1/sig_h2)
  b_h <- b_h0 + as.numeric(  (Z%*%matrix(ga, ncol = 1)) ) * (1/sig_h2)
  z_h <- rnorm(length(b_h),0,1)
  h_out <-  Q_h^(-1/2) * z_h + Q_h^(-1) * b_h

  return(h_out)
}


## posterior for gamma
q_ga <- function(Z, h, sig_h2, sig_ga2){

  Omega_ga <- (1/sig_h2) * t(Z)%*%Z +  1/sig_ga2
  w_ga <- (1/sig_h2) * t(Z)%*%h
  S_ga <- inv_sympd(Omega_ga)
  Nu_ga <- S_ga%*%w_ga

  ga_out <- as.numeric( Nu_ga + arma_chol(S_ga) %*% rnorm(length(Nu_ga),0,1) )
  return(ga_out)
}


## posterior for sigma e^2
q_sige2_FW <- function(a0, b0, Y, VAR_ind, ENV_ind, g, b, h){
  g1 <- g[VAR_ind]
  b1 <- b[VAR_ind]
  h1 <- h[ENV_ind]

  N <- length(Y)
  a1 <- a0 + N/2
  b1 <- b0 + (1/2)*sum( (Y-g1-h1-h1*b1)^2 )
  return( 1/rgamma(1, shape = a1, rate = b1) )
}


## posterior for sigma g^2
q_sigg2_I <- function(a0, b0, g){

  m <- length(g)
  a1 <- a0 + m/2
  b1 <- b0 + (1/2)* sum( g^2 )
  return( 1/rgamma(1, shape = a1, rate = b1) )
}


q_sigg2_A <- function(a0, b0, g, A1_inv){

  m <- length(g)
  a1 <- a0 + m/2
  b1 <- b0 + (1/2)* sum( g * arma_Xy(X = A1_inv, y = g) )
  return( 1/rgamma(1, shape = a1, rate = b1) )
}



## posterior for sigma b^2
q_sigb2_I <- function(a0, b0, b){

  m <- length(b)
  a1 <- a0 + m/2
  b1 <- b0 + (1/2)* sum( b^2 )
  return( 1/rgamma(1, shape = a1, rate = b1) )
}

q_sigb2_A <- function(a0, b0, b, A1_inv){

  m <- length(b)
  a1 <- a0 + m/2
  b1 <- b0 + (1/2)* sum( b * arma_Xy(X = A1_inv, y = b) )
  return( 1/rgamma(1, shape = a1, rate = b1) )
}




## posterior for sigma h^2
q_sigh2 <- function(a0, b0, h){
  n <- length(h)
  a1 <- a0 + n/2
  b1 <- b0 + (1/2) * sum( h^2 )
  return( 1/rgamma(1, shape = a1, rate = b1) )
}

q_sigh2_Z <- function(a0, b0, h, ga, Z){
  n <- length(h)
  a1 <- a0 + n/2
  b1 <- b0 + (1/2) * sum( (h - Z%*%ga)^2 )
  return( 1/rgamma(1, shape = a1, rate = b1) )
}


## posterior for sigma l^2
q_sigga2 <- function(a0, b0, ga){
  L <- length(ga)
    a1 <- a0 + L/2
    b1 <- b0 + sum( ga^2 )
  return( 1/rgamma(1, shape = a1, rate = b1) )
}


## posterior for Y
q_y_FW <- function(VAR_ind, ENV_ind, g, b, h){
  g1 <- g[VAR_ind]
  b1 <- b[VAR_ind]
  h1 <- h[ENV_ind]
  y_hat <- g1 + h1 + h1*b1
  return(y_hat)
}
















###### with spatial #####


## posterior for g
q_g_SFW_I <- function(Y, VAR_ind, ENV_ind, Phi, h, b, sig_e2, sig_g2, D, geno_ind){
  b1 <- b[VAR_ind]
  h1 <- h[ENV_ind]
  y_star <- Y - h1 - b1 * h1 - Phi
  b_g <- (1/sig_e2) * unlist(lapply(X = geno_ind, FUN = function(x){
    return( sum(y_star[x]) )
  }))

  Q_g <- (1/sig_e2) * D + (1/sig_g2)
  z_g <- rnorm( length(b_g), 0, 1 )
  g_out <-  Q_g^(-1/2) * z_g + Q_g^(-1) * b_g

  return(g_out)
}




q_g_SFW_A <- function(Y, VAR_ind, ENV_ind, Phi, h, b, sig_e2, sig_g2, D, M, La, DM, geno_ind){
  b1 <- b[VAR_ind]
  h1 <- h[ENV_ind]
  y_star <- Y - h1 - b1 * h1 - Phi
  b_g <- unlist(lapply(X = geno_ind, FUN = function(x){
    return( sum(y_star[x]) )
  }))

  rho <-  sig_e2/ sig_g2
  La1 <- 1 + rho * La
  z_g <- rnorm( length(b_g), 0, 1 )
  v_g <- sqrt(sig_e2) * arma_Xy( X = DM, y = z_g / sqrt(La1) )
  mu_g <- arma_Xy( X = DM, y = (1/La1) * arma_Xty(X = DM, y =b_g) )
  g_out <- v_g + mu_g
  return(g_out)
}





## psterior for b
q_b_SFW_I <- function(Y, VAR_ind, ENV_ind, Phi, g, h, sig_e2, sig_b2, geno_ind){
  g1 <- g[VAR_ind]
  h1 <- h[ENV_ind]
  y_star <- (Y - g1 - h1 - Phi ) * h1

  b_b <- (1/sig_e2) * unlist(lapply(X = geno_ind, FUN = function(x){
    return( sum(y_star[x]) )
  }))

  diag_b <- unlist(lapply(X = geno_ind, FUN = function(x){
    return( sum( (h1[x])^2 ) )
  }))

  Q_b <- (1/sig_e2) * diag_b + (1/sig_b2)
  z_b <- rnorm( length(b_b), 0, 1 )
  b_out <-  Q_b^(-1/2) * z_b + Q_b^(-1) * b_b

  return(b_out)
}






q_b_SFW_A <- function(Y, VAR_ind, ENV_ind, Phi, g, h, sig_e2, sig_b2, A1_inv, geno_ind){
  g1 <- g[VAR_ind]
  h1 <- h[ENV_ind]
  y_star <- (Y - g1 - h1 - Phi) * h1

  b_b <- (1/sig_e2) * unlist(lapply(X = geno_ind, FUN = function(x){
    return( sum(y_star[x]) )
  }))

  diag_b <- unlist(lapply(X = geno_ind, FUN = function(x){
    return( sum( (h1[x])^2 ) )
  }))

  Q_b <- (1/sig_e2) * diag(diag_b) + (1/sig_b2) * A1_inv
  L_b <- arma_chol( Q_b )
  w_b <- forwardsolve(l=L_b, x=b_b)
  mu_b <- forwardsolve(l=L_b, x=w_b, transpose = TRUE)
  z_b <- rnorm( length(b_b), 0, 1 )
  v_b <- forwardsolve(l=L_b, x=z_b, transpose = TRUE)
  b_out <- v_b + mu_b
  return(b_out)
}





## posterior for h
q_h_SFW <- function(Y, VAR_ind, Phi, g, b, sig_e2, sig_h2, loc_rela){
  g1 <- g[VAR_ind]
  b1 <- b[VAR_ind]
  y_star <- (1 + b1) * (Y - g1 - Phi)

  b_h <- (1/sig_e2) * unlist(lapply(X = loc_rela$loc_ind, FUN = function(x){
    return( sum(y_star[x]) )
  }))

  diag_h <- unlist(lapply(X = loc_rela$loc_ind, FUN = function(x){
    return( sum( (1 + b1[x])^2 ) )
  }))

  Q_h <- (1/sig_e2) * diag_h + (1/sig_h2)
  z_h <- rnorm(length(b_h),0,1)
  h_out <-  Q_h^(-1/2) * z_h + Q_h^(-1) * b_h

  return(h_out)
}






q_h_SFW_Z <- function(Y, VAR_ind, Phi, Z, g, b, ga, sig_e2, sig_h2, loc_rela){
  g1 <- g[VAR_ind]
  b1 <- b[VAR_ind]
  y_star <- (1 + b1) * (Y - g1 - Phi)

  b_h0 <- (1/sig_e2) * unlist(lapply(X = loc_rela$loc_ind, FUN = function(x){
    return( sum(y_star[x]) )
  }))

  diag_h <- unlist(lapply(X = loc_rela$loc_ind, FUN = function(x){
    return( sum( (1 + b1[x])^2 ) )
  }))

  Q_h <- (1/sig_e2) * diag_h + (1/sig_h2)
  b_h <- b_h0 + as.numeric(  (Z%*%matrix(ga, ncol = 1)) ) * (1/sig_h2)
  z_h <- rnorm(length(b_h),0,1)
  h_out <-  Q_h^(-1/2) * z_h + Q_h^(-1) * b_h

  return(h_out)
}




## posterior for varphi
q_varphi <- function(Y, VAR_ind, g, b, h, sig_e2, sig_psi2, theta, loc_rela, B){

  g1 <- g[VAR_ind]
  b1 <- b[VAR_ind]
  loc <- loc_rela$loc_name
  n <- length(loc)
  Varphi <- vector("list",n)
  for (j in 1:n){
    indj <- loc_rela$loc_ind[[j]]
    Yj <- Y[indj]
    gj <- g1[indj]
    bj <- b1[indj]
    hj <- h[j]
    dimj <- loc_rela$loc_dim[[j]]
    Yj_star <- Yj - gj - hj - hj * bj
    rc <- dimj[1]*dimj[2]
    F1_ind <- loc_rela$loc_pos[[j]]
    rho_j <- sig_e2/sig_psi2[j]
    D_r <- rho_j * besag_Wj_eig_val(dimj, theta[[j]])[-1]
    Fty_s <- rep(0,rc)
    Fty_s[F1_ind] <- Yj_star
    BtFty_s <- dct2mod(Fty_s, dimj[1], dimj[2])
    dt1 <- BtFty_s/(1+D_r)
    dt2 <- sqrt(sig_e2) * rnorm(rc-1,0,1)/(sqrt(1+D_r))
    if (length(F1_ind)==rc){
      Varphi[[j]] <- dt1 + dt2
    }
    if (length(F1_ind)==(rc-1)){
      F0_ind <- (1:rc)[-F1_ind]
      F0B <- B[[j]][F0_ind,]
      U <- 1 - sum( (F0B^2)/(1+D_r) )
      R <- sqrt(U)
      dt3 <- idct2mod(dt1, dimj[1], dimj[2])[F0_ind]
      dt4 <- dt3/R
      dt5 <- dt4 + sqrt(sig_e2) * rnorm(1,0,1)
      dt6 <- dt5/R
      dt7 <- rep(0,rc)
      dt7[F0_ind] <- dt6
      dt8 <- dct2mod(dt7, dimj[1], dimj[2])/(1+D_r)
      Varphi[[j]] <- dt1 + dt2 + dt8
    }
    if (length(F1_ind)<(rc-1)){
      F0_ind <- (1:rc)[-F1_ind]
      F0B <- B[[j]][F0_ind,]
      U <- diag(length(F0_ind)) - F0B %*% ( (1+D_r)^(-1) * t(F0B) )
      R <- arma_chol(U)
      dt3 <- idct2mod(dt1, dimj[1], dimj[2])[F0_ind]
      dt4 <- forwardsolve(R, dt3)
      dt5 <- dt4 + sqrt(sig_e2) * rnorm(length(F0_ind),0,1)
      dt6 <- forwardsolve(R, dt5, transpose = TRUE)
      dt7 <- rep(0,rc)
      dt7[F0_ind] <- dt6
      dt8 <- dct2mod(dt7, dimj[1], dimj[2])/(1+D_r)
      Varphi[[j]] <- dt1 + dt2 + dt8
    }
  }
  return(Varphi)
}





## posterior for sigma e^2
q_sige2_SFW <- function(a0, b0, Y, VAR_ind, ENV_ind, Phi, g, b, h){
  g1 <- g[VAR_ind]
  b1 <- b[VAR_ind]
  h1 <- h[ENV_ind]

  N <- length(Y)
  a1 <- a0 + N/2
  b1 <- b0 + (1/2)*sum( (Y-g1-h1-h1*b1-Phi)^2 )
  return( 1/rgamma(1, shape = a1, rate = b1) )
}






## posterior for sigma psi^2
q_sigpsi2 <- function(a0, b0, Varphi, theta, loc_rela){
  loc <- loc_rela$loc_name
  n <- length(loc)
  sig2 <- rep(0,n)
  for (j in 1:n){
    dimj <- loc_rela$loc_dim[[j]]
    varphi_j <- Varphi[[j]]
    Dj_vec <- besag_Wj_eig_val(dimj, theta[[j]])
    Dj_vec <- Dj_vec[-1]
    a1 <- a0[j] + (1/2)* (dimj[1]*dimj[2]-1)
    b1 <- b0[j] + (1/2)* sum(Dj_vec*(varphi_j)*(varphi_j))
    sig2[j] <- 1/rgamma(1, shape = a1, rate = b1)
  }
  return(sig2)
}



## posterior for theta
q_theta_trans_rw <- function(theta, sig_psi2, Varphi, loc_rela, kappa){
  n <- length(theta)
  rej_ind <- rep(0,n)
  theta1 <- theta
  s <- qnorm(theta)
  for (j in 1:n){
    sj <- rnorm( 1, s[j], sqrt(kappa[j]) )
    tj <- pnorm(sj)
    dimj <- loc_rela$loc_dim[[j]]
    varphi_j <- Varphi[[j]]
    Dj_vec1 <- besag_Wj_eig_val(dimj, tj)
    Dj_vec1 <- Dj_vec1[-1]
    Dj_vec2 <- besag_Wj_eig_val(dimj, theta[j])
    Dj_vec2 <- Dj_vec2[-1]
    log_det_Dj1 <- sum(log(Dj_vec1))
    log_det_Dj2 <- sum(log(Dj_vec2))
    S1 <- (1/2)*log_det_Dj1 - 1/(2*sig_psi2[j]) * sum(Dj_vec1*varphi_j*varphi_j)
    S2 <- (1/2)*log_det_Dj2 - 1/(2*sig_psi2[j]) * sum(Dj_vec2*varphi_j*varphi_j)
    S3 <- dnorm(sj, log = TRUE)
    S4 <- dnorm(s[j], log = TRUE)
    alpha_j <- min(1, exp(S1-S2+S3-S4) )
    test_stat <- runif(1)
    if (test_stat < alpha_j){
      theta1[j] <- tj
      rej_ind[j] <- 1
    }
  }
  out <- list(theta1 = theta1, rej_ind = rej_ind)
  return(out)
}




## posterior for Y
q_y_SFW <- function(VAR_ind, ENV_ind, Phi, g, b, h){
  g1 <- g[VAR_ind]
  b1 <- b[VAR_ind]
  h1 <- h[ENV_ind]
  y_hat <- g1 + h1 + h1*b1 + Phi
  return(y_hat)
}












