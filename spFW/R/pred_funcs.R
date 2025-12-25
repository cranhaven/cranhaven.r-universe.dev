###################### Prediction functions #########################

FW_pred <- function(VAR2, ENV2, muhat,
                    ghat, bhat, hhat, sige2hat, save_int){
  gg <- paste("g",VAR2,sep="-")
  bb <- paste("b",VAR2,sep="-")
  hh <- paste("h",ENV2,sep="-")

  N2 <- length(VAR2)
  M2 <- length(ghat[,1])

  PY2 <- matrix(0, ncol = N2, nrow = M2)
  if (save_int==TRUE){
    ERR <- matrix(0, ncol = N2, nrow = M2)
  }

  for (k in 1:N2){
    ind_g <- which(gg[k]==colnames(ghat))
    ind_b <- which(bb[k]==colnames(bhat))
    ind_h <- which(hh[k]==colnames(hhat))
    PY2[,k] <-  muhat + ghat[,ind_g] + hhat[,ind_h] * (1+bhat[,ind_b])
    if (save_int==TRUE){
      ERR[,k] <- rnorm(M2, 0, 1) * sqrt( sige2hat  )
    }
  }


  Py2 <- apply(PY2,2,mean)

  if (save_int==TRUE){
    PY3 <- PY2 + ERR
    py2_int <- apply(PY3,2,quantile,probs=c(0.025, 0.05, 0.95, 0.975))
    Outputs <- list(PY = Py2, PY_CI = py2_int)
  } else {
    Outputs <- Py2
  }

  return(Outputs)
}







FW_pred_new <- function(VAR, VAR2, ENV2, muhat,
                        ghat, bhat, hhat, sige2hat, sigg2hat, sigb2hat,
                        kin_info, A, save_int){

  gg <- paste("g",VAR2,sep="-")
  bb <- paste("b",VAR2,sep="-")
  hh <- paste("h",ENV2,sep="-")

  g_ind <- names(table(VAR))
  g_ind2 <- names(table(VAR2))
  g_ind_diff <- setdiff(g_ind2, g_ind)

  N2 <- length(VAR2)
  M2 <- length(ghat[,1])
  n1 <- length(g_ind)
  n2 <- length(g_ind_diff)

  ghat2 <- matrix(0, ncol = n2, nrow = M2)
  colnames(ghat2) <- paste("g",g_ind_diff,sep="-")
  bhat2 <- matrix(0, ncol = n2, nrow = M2)
  colnames(bhat2) <- paste("b",g_ind_diff,sep="-")

  if (kin_info == TRUE){
    A_names <- rownames(A)
    indA1 <- NULL
    indA2 <- NULL
    for (i in 1:length(g_ind)){
      ind <- which(g_ind[i]==A_names)
      indA1 <- c(indA1,ind)
    }
    for (i in 1:length(g_ind_diff)){
      ind <- which(g_ind_diff[i]==A_names)
      indA2 <- c(indA2,ind)
    }
    A11 <- A[indA1, indA1]
    A22 <- A[indA2, indA2]
    A12 <- A[indA1, indA2]
    A21 <- t(A12)
    inv_A11 <- inv_sympd(A11)
    B1 <- A21 %*% inv_A11
    B2 <- A22 - A21 %*% inv_A11 %*% A12
    B3 <- arma_chol(B2)

    for (i in 1:M2){
      ghat2[i,] <- B1%*%ghat[i,] + B3%*%rnorm(n2,0,1) * sqrt(sigg2hat[i])
      bhat2[i,] <- B1%*%bhat[i,] + B3%*%rnorm(n2,0,1) * sqrt(sigb2hat[i])
    }

  } else {
    for (i in 1:M2){
      ghat2[i,] <- rnorm(n2,0,1) * sqrt(sigg2hat[i])
      bhat2[i,] <- rnorm(n2,0,1) * sqrt(sigb2hat[i])
    }
  }

  ghat <- cbind(ghat, ghat2)
  bhat <- cbind(bhat, bhat2)


  PY2 <- matrix(0, ncol = N2, nrow = M2)
  if (save_int==TRUE){
    ERR <- matrix(0, ncol = N2, nrow = M2)
  }

  for (k in 1:N2){
    ind_g <- which(gg[k]==colnames(ghat))
    ind_b <- which(bb[k]==colnames(bhat))
    ind_h <- which(hh[k]==colnames(hhat))
    PY2[,k] <-  muhat + ghat[,ind_g] + hhat[,ind_h] * (1+bhat[,ind_b])
    if (save_int==TRUE){
      ERR[,k] <- rnorm(M2, 0, 1) * sqrt( sige2hat  )
    }
  }


  Py2 <- apply(PY2,2,mean)

  if (save_int==TRUE){
    PY3 <- PY2 + ERR
    py2_int <- apply(PY3,2,quantile,probs=c(0.025, 0.05, 0.95, 0.975))
    Outputs <- list(PY = Py2, PY_CI = py2_int)
  } else {
    Outputs <- Py2
  }

  return(Outputs)

}






###################### Prediction functions #########################

SFW_I_pred <- function(M, burn_in, thin,
                       Y, VAR, ENV, COOR, ENV2, COOR2,Phi0,
                       g0, b0, h0,
                       se2_0, sg2_0, sb2_0, sh2_0, spsi2_0,
                       ae0, be0, ag0, bg0,
                       ab0, bb0, ah0, bh0,
                       apsi0, bpsi0, theta0, kappa){
  muY <- mean(Y)
  Yc <- Y - mean(Y)
  N <- length(Y)
  N2 <- length(ENV2)
  N_para <- length(g0) + length(b0) + length(h0) +
    length(se2_0) + length(sg2_0) +
    length(sb2_0) + length(sh2_0) +
    length(spsi2_0) + length(theta0)

  M_use <- ceiling( (M - burn_in) / thin )
  gibbs_sample <- matrix(0, ncol = N_para, nrow = M_use)
  Phi2_hat <- matrix(0, ncol = N2, nrow = M_use)
  loc_rela <- loc_relation1(ENV, ENV2, COOR, COOR2)
  loc_rela2 <- loc_relation2(ENV, ENV2, COOR, COOR2)
  Rej_theta <- matrix(0, ncol = length(theta0), nrow = M)
  n <- length(loc_rela$loc_name)
  B <- besag_B(loc_rela)

  g_ind <- names(table(VAR))
  b_ind <- g_ind
  h_ind <- names(table(ENV))
  g0 <- g_change(g0, g_ind)
  b0 <- b_change(b0, b_ind)
  h0 <- h_change(h0, h_ind)
  VAR_ind <- find_pos(VAR, g_ind)
  ENV_ind <- find_pos(ENV, h_ind)

  D0 <- as.numeric(table(VAR))
  geno_ind0 <- Geno_ind(VAR)

  for (i in 1:M){

    g1 <- q_g_SFW_I(Yc, VAR_ind, ENV_ind, Phi0, h0, b0, se2_0, sg2_0, D0, geno_ind0)
    b1 <- q_b_SFW_I(Yc, VAR_ind, ENV_ind, Phi0, g1, h0, se2_0, sb2_0, geno_ind0)
    h1 <- q_h_SFW(Yc, VAR_ind, Phi0, g1, b1, se2_0, sh2_0, loc_rela)
    Varphi1 <- q_varphi(Yc, VAR_ind, g1, b1, h1, se2_0, spsi2_0, theta0, loc_rela, B)
    Psi1 <- varphi2psi(Varphi1, n, B)
    Phi1 <- psi2phi(Psi1, loc_rela, N)
    Phi2 <- psi2phi(Psi1, loc_rela2, N2)
    se2_1 <- q_sige2_SFW(ae0, be0, Yc, VAR_ind, ENV_ind, Phi1, g1, b1, h1)
    sg2_1 <- q_sigg2_I(ag0, bg0, g1)
    sb2_1 <- q_sigb2_I(ab0, bb0, b1)
    sh2_1 <- q_sigh2(ah0, bh0, h1)
    spsi2_1 <- q_sigpsi2(apsi0, bpsi0, Varphi1, theta0, loc_rela)
    q_theta_out <- q_theta_trans_rw(theta0, spsi2_1, Varphi1, loc_rela, kappa)
    theta1 <- q_theta_out[[1]]
    Rej_theta[i,] <- q_theta_out[[2]]

    if ( (i > burn_in) & ((i-burn_in)%%thin == 1) ){
      i1 <- (i - burn_in - 1)/thin + 1
      Phi2_hat[i1,] <- Phi2
      gibbs_sample[i1, ] <- c(g1,b1,h1,se2_1,sg2_1,sb2_1,sh2_1,spsi2_1,theta1)
    }

    g0 <- g1
    b0 <- b1
    h0 <- h1
    Phi0 <- Phi1
    se2_0 <- se2_1
    sg2_0 <- sg2_1
    sb2_0 <- sb2_1
    sh2_0 <- sh2_1
    spsi2_0 <- spsi2_1
    theta0 <- theta1

    if ( (i%%100==0) & (i>(burn_in/2)) ){
      acc_rate <- apply(Rej_theta[(1:100)+(i-100),],2,mean)
      decrease_var_ind <- which(acc_rate < 0.1)
      increase_var_ind <- which(acc_rate > 0.6)
      kappa[decrease_var_ind] <- (1/2)*kappa[decrease_var_ind]
      kappa[increase_var_ind] <- (2)*kappa[increase_var_ind]
    }

    if ( i%%250 == 0 ){
      cat("iter:", i, "; \n")
    }
  }
  colnames(gibbs_sample) <- c(paste("g", g_ind, sep="-"),
                              paste("b", b_ind, sep="-"),
                              paste("h", h_ind, sep="-"),
                              "var_e", "var_g", "var_b", "var_h",
                              paste("var_psi",h_ind,sep="-"),
                              paste("theta",h_ind,sep="-"))

  Output <- list(gsamps = gibbs_sample, phi2hat =  Phi2_hat)

  return(Output)
}





SFW_A_pred <- function(M, burn_in, thin,
                       Y, VAR, ENV, COOR, ENV2, COOR2, A, Phi0,
                       g0, b0, h0,
                       se2_0, sg2_0, sb2_0, sh2_0, spsi2_0,
                       ae0, be0, ag0, bg0,
                       ab0, bb0, ah0, bh0,
                       apsi0, bpsi0, theta0, kappa){
  muY <- mean(Y)
  Yc <- Y - mean(Y)
  N <- length(Y)
  N2 <- length(ENV2)
  N_para <- length(g0) + length(b0) + length(h0) +
    length(se2_0) + length(sg2_0) +
    length(sb2_0) + length(sh2_0) +
    length(spsi2_0) + length(theta0)

  M_use <- ceiling( (M - burn_in) / thin )
  gibbs_sample <- matrix(0, ncol = N_para, nrow = M_use)
  Phi2_hat <- matrix(0, ncol = N2, nrow = M_use)
  loc_rela <- loc_relation1(ENV, ENV2, COOR, COOR2)
  loc_rela2 <- loc_relation2(ENV, ENV2, COOR, COOR2)
  Rej_theta <- matrix(0, ncol = length(theta0), nrow = M)
  n <- length(loc_rela$loc_name)
  B <- besag_B(loc_rela)

  g_ind <- names(table(VAR))
  b_ind <- g_ind
  h_ind <- names(table(ENV))
  A1 <- A_change(A, g_ind)
  A1_inv <- inv_sympd(A1)
  g0 <- g_change(g0, g_ind)
  b0 <- b_change(b0, b_ind)
  h0 <- h_change(h0, h_ind)
  VAR_ind <- find_pos(VAR, g_ind)
  ENV_ind <- find_pos(ENV, h_ind)

  D0 <- as.numeric(table(VAR))
  E0 <- D0^(-1/2) * t( D0^(-1/2) * A1_inv )
  eigen_E0 <- eigen(E0)
  La0 <- eigen_E0$values
  M0 <- eigen_E0$vectors
  DM0 <- D0^(-1/2) * M0
  geno_ind0 <- Geno_ind(VAR)

  for (i in 1:M){

    g1 <- q_g_SFW_A(Yc, VAR_ind, ENV_ind, Phi0, h0, b0, se2_0, sg2_0, D0, M0, La0, DM0, geno_ind0)
    b1 <- q_b_SFW_A(Yc, VAR_ind, ENV_ind, Phi0, g1, h0, se2_0, sb2_0, A1_inv, geno_ind0)
    h1 <- q_h_SFW(Yc, VAR_ind, Phi0, g1, b1, se2_0, sh2_0, loc_rela)
    Varphi1 <- q_varphi(Yc, VAR_ind, g1, b1, h1, se2_0, spsi2_0, theta0, loc_rela, B)
    Psi1 <- varphi2psi(Varphi1, n, B)
    Phi1 <- psi2phi(Psi1, loc_rela, N)
    Phi2 <- psi2phi(Psi1, loc_rela2, N2)
    se2_1 <- q_sige2_SFW(ae0, be0, Yc, VAR_ind, ENV_ind, Phi1, g1, b1, h1)
    sg2_1 <- q_sigg2_A(ag0, bg0, g1, A1_inv)
    sb2_1 <- q_sigb2_A(ab0, bb0, b1, A1_inv)
    sh2_1 <- q_sigh2(ah0, bh0, h1)
    spsi2_1 <- q_sigpsi2(apsi0, bpsi0, Varphi1, theta0, loc_rela)
    q_theta_out <- q_theta_trans_rw(theta0, spsi2_1, Varphi1, loc_rela, kappa)
    theta1 <- q_theta_out[[1]]
    Rej_theta[i,] <- q_theta_out[[2]]

    if ( (i > burn_in) & ((i-burn_in)%%thin == 1) ){
      i1 <- (i - burn_in - 1)/thin + 1
      Phi2_hat[i1,] <- Phi2
      gibbs_sample[i1, ] <- c(g1,b1,h1,se2_1,sg2_1,sb2_1,sh2_1,spsi2_1,theta1)
    }

    g0 <- g1
    b0 <- b1
    h0 <- h1
    Phi0 <- Phi1
    se2_0 <- se2_1
    sg2_0 <- sg2_1
    sb2_0 <- sb2_1
    sh2_0 <- sh2_1
    spsi2_0 <- spsi2_1
    theta0 <- theta1

    if ( (i%%100==0) & (i>(burn_in/2)) ){
      acc_rate <- apply(Rej_theta[(1:100)+(i-100),],2,mean)
      decrease_var_ind <- which(acc_rate < 0.1)
      increase_var_ind <- which(acc_rate > 0.6)
      kappa[decrease_var_ind] <- (1/2)*kappa[decrease_var_ind]
      kappa[increase_var_ind] <- (2)*kappa[increase_var_ind]
    }

    if ( i%%250 == 0 ){
      cat("iter:", i, "; \n")
    }
  }
  colnames(gibbs_sample) <- c(paste("g", g_ind, sep="-"),
                              paste("b", b_ind, sep="-"),
                              paste("h", h_ind, sep="-"),
                              "var_e", "var_g", "var_b", "var_h",
                              paste("var_psi",h_ind,sep="-"),
                              paste("theta",h_ind,sep="-"))


  Output <- list(gsamps = gibbs_sample, phi2hat =  Phi2_hat)

  return(Output)
}








SFW_pred <- function(VAR2, ENV2, muhat, phi2hat,
                     ghat, bhat, hhat, sige2hat, save_int){
  gg <- paste("g",VAR2,sep="-")
  bb <- paste("b",VAR2,sep="-")
  hh <- paste("h",ENV2,sep="-")

  N2 <- length(VAR2)
  M2 <- length(ghat[,1])

  PY2 <- matrix(0, ncol = N2, nrow = M2)
  if (save_int==TRUE){
    ERR <- matrix(0, ncol = N2, nrow = M2)
  }

  for (k in 1:N2){
    ind_g <- which(gg[k]==colnames(ghat))
    ind_b <- which(bb[k]==colnames(bhat))
    ind_h <- which(hh[k]==colnames(hhat))
    PY2[,k] <-  muhat + ghat[,ind_g] + hhat[,ind_h] * (1+bhat[,ind_b]) +
      phi2hat[,k]
    if (save_int==TRUE){
      ERR[,k] <- rnorm(M2, 0, 1) * sqrt( sige2hat  )
    }
  }


  Py2 <- apply(PY2,2,mean)

  if (save_int==TRUE){
    PY3 <- PY2 + ERR
    py2_int <- apply(PY3,2,quantile,probs=c(0.025, 0.05, 0.95, 0.975))
    Outputs <- list(PY = Py2, PY_CI = py2_int)
  } else {
    Outputs <- Py2
  }

  return(Outputs)
}








SFW_pred_new <- function(VAR, VAR2, ENV2, muhat, phi2hat,
                         ghat, bhat, hhat, sige2hat, sigg2hat, sigb2hat,
                         kin_info, A, save_int){

  gg <- paste("g",VAR2,sep="-")
  bb <- paste("b",VAR2,sep="-")
  hh <- paste("h",ENV2,sep="-")

  g_ind <- names(table(VAR))
  g_ind2 <- names(table(VAR2))
  g_ind_diff <- setdiff(g_ind2, g_ind)

  N2 <- length(VAR2)
  M2 <- length(ghat[,1])
  n1 <- length(g_ind)
  n2 <- length(g_ind_diff)

  ghat2 <- matrix(0, ncol = n2, nrow = M2)
  colnames(ghat2) <- paste("g",g_ind_diff,sep="-")
  bhat2 <- matrix(0, ncol = n2, nrow = M2)
  colnames(bhat2) <- paste("b",g_ind_diff,sep="-")

  if (kin_info == TRUE){
    A_names <- rownames(A)
    indA1 <- NULL
    indA2 <- NULL
    for (i in 1:length(g_ind)){
      ind <- which(g_ind[i]==A_names)
      indA1 <- c(indA1,ind)
    }
    for (i in 1:length(g_ind_diff)){
      ind <- which(g_ind_diff[i]==A_names)
      indA2 <- c(indA2,ind)
    }
    A11 <- A[indA1, indA1]
    A22 <- A[indA2, indA2]
    A12 <- A[indA1, indA2]
    A21 <- t(A12)
    inv_A11 <- inv_sympd(A11)
    B1 <- A21 %*% inv_A11
    B2 <- A22 - A21 %*% inv_A11 %*% A12
    B3 <- arma_chol(B2)

    for (i in 1:M2){
      ghat2[i,] <- B1%*%ghat[i,] + B3%*%rnorm(n2,0,1) * sqrt(sigg2hat[i])
      bhat2[i,] <- B1%*%bhat[i,] + B3%*%rnorm(n2,0,1) * sqrt(sigb2hat[i])
    }

  } else {
    for (i in 1:M2){
      ghat2[i,] <- rnorm(n2,0,1) * sqrt(sigg2hat[i])
      bhat2[i,] <- rnorm(n2,0,1) * sqrt(sigb2hat[i])
    }
  }

  ghat <- cbind(ghat, ghat2)
  bhat <- cbind(bhat, bhat2)


  PY2 <- matrix(0, ncol = N2, nrow = M2)
  if (save_int==TRUE){
    ERR <- matrix(0, ncol = N2, nrow = M2)
  }

  for (k in 1:N2){
    ind_g <- which(gg[k]==colnames(ghat))
    ind_b <- which(bb[k]==colnames(bhat))
    ind_h <- which(hh[k]==colnames(hhat))
    PY2[,k] <-  muhat + ghat[,ind_g] + hhat[,ind_h] * (1+bhat[,ind_b]) +
      phi2hat[,k]
    if (save_int==TRUE){
      ERR[,k] <- rnorm(M2, 0, 1) * sqrt( sige2hat  )
    }
  }


  Py2 <- apply(PY2,2,mean)

  if (save_int==TRUE){
    PY3 <- PY2 + ERR
    py2_int <- apply(PY3,2,quantile,probs=c(0.025, 0.05, 0.95, 0.975))
    Outputs <- list(PY = Py2, PY_CI = py2_int)
  } else {
    Outputs <- Py2
  }

  return(Outputs)

}


