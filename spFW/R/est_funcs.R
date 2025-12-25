################## Estimation functions ########################

FW_I <- function(M, burn_in, thin,
                 Y, VAR, ENV,
                 g0, b0, h0,
                 se2_0, sg2_0, sb2_0, sh2_0,
                 ae0, be0, ag0, bg0,
                 ab0, bb0, ah0, bh0){




  muY <- mean(Y)
  Yc <- Y - mean(Y)
  N <- length(Y)
  N_para <- length(g0) + length(b0) + length(h0) +
    length(se2_0) + length(sg2_0) +
    length(sb2_0) + length(sh2_0)

  M_use <- ceiling( (M - burn_in) / thin )
  gibbs_sample <- matrix(0, ncol = N_para, nrow = M_use)
  Y_hat <- matrix(0, ncol = N, nrow = M_use)

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
  loc_rela <- loc_relation0(ENV)

  for (i in 1:M){

    g1 <- q_g_FW_I(Yc, VAR_ind, ENV_ind, h0, b0, se2_0, sg2_0, D0, geno_ind0)
    b1 <- q_b_FW_I(Yc, VAR_ind, ENV_ind, g1, h0, se2_0, sb2_0, geno_ind0)
    h1 <- q_h_FW(Yc, VAR_ind, g1, b1, se2_0, sh2_0, loc_rela)
    se2_1 <- q_sige2_FW(ae0, be0, Yc, VAR_ind, ENV_ind, g1, b1, h1)
    sg2_1 <- q_sigg2_I(ag0, bg0, g1)
    sb2_1 <- q_sigb2_I(ab0, bb0, b1)
    sh2_1 <- q_sigh2(ah0, bh0, h1)
    y_hat <- q_y_FW(VAR_ind, ENV_ind, g1, b1, h1)

    if ( (i > burn_in) & ((i-burn_in)%%thin == 1) ){
      i1 <- (i - burn_in - 1)/thin + 1
      Y_hat[i1, ] <- y_hat
      gibbs_sample[i1, ] <- c(g1,b1,h1,se2_1,sg2_1,sb2_1,sh2_1)
    }

    g0 <- g1
    b0 <- b1
    h0 <- h1
    se2_0 <- se2_1
    sg2_0 <- sg2_1
    sb2_0 <- sb2_1
    sh2_0 <- sh2_1

    if ( i%%1000 == 0 ){
      cat("iter:", i, "; \n")
    }

  }
  colnames(gibbs_sample) <- c(paste("g",g_ind,sep="-"),
                              paste("b",b_ind,sep="-"),
                              paste("h",h_ind,sep="-"),
                              "var_e", "var_g", "var_b", "var_h")



  Y_hat1 <- apply(Y_hat, 2, mean) + muY

  Output <- list(gsamps = gibbs_sample, yhat = Y_hat1)

  return(Output)
}








FW_A <- function(M, burn_in, thin,
                 Y, VAR, ENV,
                 g0, b0, h0,
                 se2_0, sg2_0, sb2_0, sh2_0, A,
                 ae0, be0, ag0, bg0,
                 ab0, bb0, ah0, bh0){




  muY <- mean(Y)
  Yc <- Y - mean(Y)
  N <- length(Y)
  N_para <- length(g0) + length(b0) + length(h0) +
    length(se2_0) + length(sg2_0) +
    length(sb2_0) + length(sh2_0)

  M_use <- ceiling( (M - burn_in) / thin )
  gibbs_sample <- matrix(0, ncol = N_para, nrow = M_use)
  Y_hat <- matrix(0, ncol = N, nrow = M_use)

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
  loc_rela <- loc_relation0(ENV)

  for (i in 1:M){

    g1 <- q_g_FW_A(Yc, VAR_ind, ENV_ind, h0, b0, se2_0, sg2_0, D0, M0, La0, DM0, geno_ind0)
    b1 <- q_b_FW_A(Yc, VAR_ind, ENV_ind, g1, h0, se2_0, sb2_0, A1_inv, geno_ind0)
    h1 <- q_h_FW(Yc, VAR_ind, g1, b1, se2_0, sh2_0, loc_rela)
    se2_1 <- q_sige2_FW(ae0, be0, Yc, VAR_ind, ENV_ind, g1, b1, h1)
    sg2_1 <- q_sigg2_A(ag0, bg0, g1, A1_inv)
    sb2_1 <- q_sigb2_A(ab0, bb0, b1, A1_inv)
    sh2_1 <- q_sigh2(ah0, bh0, h1)
    y_hat <- q_y_FW(VAR_ind, ENV_ind, g1, b1, h1)

    if ( (i > burn_in) & ((i-burn_in)%%thin == 1) ){
      i1 <- (i - burn_in - 1)/thin + 1
      Y_hat[i1, ] <- y_hat
      gibbs_sample[i1, ] <- c(g1,b1,h1,se2_1,sg2_1,sb2_1,sh2_1)
    }

    g0 <- g1
    b0 <- b1
    h0 <- h1
    se2_0 <- se2_1
    sg2_0 <- sg2_1
    sb2_0 <- sb2_1
    sh2_0 <- sh2_1


    if ( i%%500 == 0 ){
      cat("iter:", i, "; \n")
    }

  }
  colnames(gibbs_sample) <- c(paste("g",g_ind,sep="-"),
                              paste("b",b_ind,sep="-"),
                              paste("h",h_ind,sep="-"),
                              "var_e", "var_g", "var_b", "var_h")


  Y_hat1 <- apply(Y_hat, 2, mean) + muY

  Output <- list(gsamps = gibbs_sample, yhat = Y_hat1)

  return(Output)
}







FW_I_Z <- function(M, burn_in, thin,
                   Y, VAR, ENV, Z,
                   g0, b0, h0, ga0,
                   se2_0, sg2_0, sb2_0, sh2_0, sga2_0,
                   ae0, be0, ag0, bg0,
                   ab0, bb0, ah0, bh0,
                   aga0, bga0){




  muY <- mean(Y)
  Yc <- Y - mean(Y)
  N <- length(Y)
  N_para <- length(g0) + length(b0) + length(h0) +
    length(ga0) + length(se2_0) + length(sg2_0) +
    length(sb2_0) + length(sh2_0) + length(sga2_0)

  M_use <- ceiling( (M - burn_in) / thin )
  gibbs_sample <- matrix(0, ncol = N_para, nrow = M_use)
  Y_hat <- matrix(0, ncol = N, nrow = M_use)

  g_ind <- names(table(VAR))
  b_ind <- g_ind
  h_ind <- names(table(ENV))

  g0 <- g_change(g0, g_ind)
  b0 <- b_change(b0, b_ind)
  h0 <- h_change(h0, h_ind)
  Z <- Z_change(Z, h_ind)
  VAR_ind <- find_pos(VAR, g_ind)
  ENV_ind <- find_pos(ENV, h_ind)

  D0 <- as.numeric(table(VAR))

  geno_ind0 <- Geno_ind(VAR)
  loc_rela <- loc_relation0(ENV)

  for (i in 1:M){

    g1 <- q_g_FW_I(Yc, VAR_ind, ENV_ind, h0, b0, se2_0, sg2_0, D0, geno_ind0)
    b1 <- q_b_FW_I(Yc, VAR_ind, ENV_ind, g1, h0, se2_0, sb2_0, geno_ind0)
    h1 <- q_h_FW_Z(Yc, VAR_ind, Z, g1, b1, ga0, se2_0, sh2_0, loc_rela)
    ga1 <- q_ga(Z, h1, sh2_0, sga2_0)
    se2_1 <- q_sige2_FW(ae0, be0, Yc, VAR_ind, ENV_ind, g1, b1, h1)
    sg2_1 <- q_sigg2_I(ag0, bg0, g1)
    sb2_1 <- q_sigb2_I(ab0, bb0, b1)
    sh2_1 <- q_sigh2_Z(ah0, bh0, h1, ga1, Z)
    sga2_1 <- q_sigga2(aga0, bga0, ga1)
    y_hat <- q_y_FW(VAR_ind, ENV_ind, g1, b1, h1)

    if ( (i > burn_in) & ((i-burn_in)%%thin == 1) ){
      i1 <- (i - burn_in - 1)/thin + 1
      Y_hat[i1, ] <- y_hat
      gibbs_sample[i1, ] <- c(g1,b1,h1,ga1,se2_1,sg2_1,sb2_1,sh2_1,sga2_1)
    }

    g0 <- g1
    b0 <- b1
    h0 <- h1
    ga0 <- ga1
    se2_0 <- se2_1
    sg2_0 <- sg2_1
    sb2_0 <- sb2_1
    sh2_0 <- sh2_1
    sga2_0 <- sga2_1

    if ( i%%1000 == 0 ){
      cat("iter:", i, "; \n")
    }

  }
  colnames(gibbs_sample) <- c(paste("g",g_ind,sep="-"),
                              paste("b",b_ind,sep="-"),
                              paste("h",h_ind,sep="-"),
                              paste("gamma",(1:length(ga1)),sep="-"),
                              "var_e", "var_g", "var_b", "var_h", "var_ga")


  Y_hat1 <- apply(Y_hat, 2, mean) + muY

  Output <- list(gsamps = gibbs_sample, yhat = Y_hat1)

  return(Output)
}








FW_A_Z <- function(M, burn_in, thin,
                   Y, VAR, ENV, Z,
                   g0, b0, h0, ga0,
                   se2_0, sg2_0, sb2_0, sh2_0, sga2_0, A,
                   ae0, be0, ag0, bg0,
                   ab0, bb0, ah0, bh0,
                   aga0, bga0){




  muY <- mean(Y)
  Yc <- Y - mean(Y)
  N <- length(Y)
  N_para <- length(g0) + length(b0) + length(h0) +
    length(ga0) + length(se2_0) + length(sg2_0) +
    length(sb2_0) + length(sh2_0) + length(sga2_0)

  M_use <- ceiling( (M - burn_in) / thin )
  gibbs_sample <- matrix(0, ncol = N_para, nrow = M_use)
  Y_hat <- matrix(0, ncol = N, nrow = M_use)

  g_ind <- names(table(VAR))
  b_ind <- g_ind
  h_ind <- names(table(ENV))
  Z <- Z_change(Z, h_ind)
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
  loc_rela <- loc_relation0(ENV)

  for (i in 1:M){

    g1 <- q_g_FW_A(Yc, VAR_ind, ENV_ind, h0, b0, se2_0, sg2_0, D0, M0, La0, DM0, geno_ind0)
    b1 <- q_b_FW_A(Yc, VAR_ind, ENV_ind, g1, h0, se2_0, sb2_0, A1_inv, geno_ind0)
    h1 <- q_h_FW_Z(Yc, VAR_ind, Z, g1, b1, ga0, se2_0, sh2_0, loc_rela)
    ga1 <- q_ga(Z, h1, sh2_0, sga2_0)
    se2_1 <- q_sige2_FW(ae0, be0, Yc, VAR_ind, ENV_ind, g1, b1, h1)
    sg2_1 <- q_sigg2_A(ag0, bg0, g1, A1_inv)
    sb2_1 <- q_sigb2_A(ab0, bb0, b1, A1_inv)
    sh2_1 <- q_sigh2_Z(ah0, bh0, h1, ga1, Z)
    sga2_1 <- q_sigga2(aga0, bga0, ga1)
    y_hat <- q_y_FW(VAR_ind, ENV_ind, g1, b1, h1)

    if ( (i > burn_in) & ((i-burn_in)%%thin == 1) ){
      i1 <- (i - burn_in - 1)/thin + 1
      Y_hat[i1, ] <- y_hat
      gibbs_sample[i1, ] <- c(g1,b1,h1,ga1,se2_1,sg2_1,sb2_1,sh2_1,sga2_1)
    }

    g0 <- g1
    b0 <- b1
    h0 <- h1
    ga0 <- ga1
    se2_0 <- se2_1
    sg2_0 <- sg2_1
    sb2_0 <- sb2_1
    sh2_0 <- sh2_1
    sga2_0 <- sga2_1

    if ( i%%500 == 0 ){
      cat("iter:", i, "; \n")
    }

  }
  colnames(gibbs_sample) <- c(paste("g",g_ind,sep="-"),
                              paste("b",b_ind,sep="-"),
                              paste("h",h_ind,sep="-"),
                              paste("gamma",(1:length(ga1)),sep="-"),
                              "var_e", "var_g", "var_b", "var_h", "var_ga")



  Y_hat1 <- apply(Y_hat, 2, mean) + muY

  Output <- list(gsamps = gibbs_sample, yhat = Y_hat1)

  return(Output)
}







SFW_I <- function(M, burn_in, thin,
                  Y, VAR, ENV, COOR, Phi0,
                  g0, b0, h0,
                  se2_0, sg2_0, sb2_0, sh2_0, spsi2_0,
                  ae0, be0, ag0, bg0,
                  ab0, bb0, ah0, bh0,
                  apsi0, bpsi0, theta0, kappa){
  muY <- mean(Y)
  Yc <- Y - mean(Y)
  N <- length(Y)
  N_para <- length(g0) + length(b0) + length(h0) +
    length(se2_0) + length(sg2_0) +
    length(sb2_0) + length(sh2_0) +
    length(spsi2_0) + length(theta0)

  M_use <- ceiling( (M - burn_in) / thin )
  gibbs_sample <- matrix(0, ncol = N_para, nrow = M_use)
  Y_hat <- matrix(0, ncol = N, nrow = M_use)
  loc_rela <- loc_relation(ENV, COOR)
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
    se2_1 <- q_sige2_SFW(ae0, be0, Yc, VAR_ind, ENV_ind, Phi1, g1, b1, h1)
    sg2_1 <- q_sigg2_I(ag0, bg0, g1)
    sb2_1 <- q_sigb2_I(ab0, bb0, b1)
    sh2_1 <- q_sigh2(ah0, bh0, h1)
    spsi2_1 <- q_sigpsi2(apsi0, bpsi0, Varphi1, theta0, loc_rela)
    q_theta_out <- q_theta_trans_rw(theta0, spsi2_1, Varphi1, loc_rela, kappa)
    theta1 <- q_theta_out[[1]]
    Rej_theta[i,] <- q_theta_out[[2]]
    y_hat <- q_y_SFW(VAR_ind, ENV_ind, Phi1, g1, b1, h1)

    if ( (i > burn_in) & ((i-burn_in)%%thin == 1) ){
      i1 <- (i - burn_in - 1)/thin + 1
      Y_hat[i1, ] <- y_hat
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


  Y_hat1 <- apply(Y_hat, 2, mean) + muY

  Output <- list(gsamps = gibbs_sample, yhat = Y_hat1)

  return(Output)
}






SFW_A <- function(M, burn_in, thin,
                  Y, VAR, ENV, COOR, A, Phi0,
                  g0, b0, h0,
                  se2_0, sg2_0, sb2_0, sh2_0, spsi2_0,
                  ae0, be0, ag0, bg0,
                  ab0, bb0, ah0, bh0,
                  apsi0, bpsi0, theta0, kappa){
  muY <- mean(Y)
  Yc <- Y - mean(Y)
  N <- length(Y)
  N_para <- length(g0) + length(b0) + length(h0) +
    length(se2_0) + length(sg2_0) +
    length(sb2_0) + length(sh2_0) +
    length(spsi2_0) + length(theta0)

  M_use <- ceiling( (M - burn_in) / thin )
  gibbs_sample <- matrix(0, ncol = N_para, nrow = M_use)
  Y_hat <- matrix(0, ncol = N, nrow = M_use)
  loc_rela <- loc_relation(ENV, COOR)
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
    se2_1 <- q_sige2_SFW(ae0, be0, Yc, VAR_ind, ENV_ind, Phi1, g1, b1, h1)
    sg2_1 <- q_sigg2_A(ag0, bg0, g1, A1_inv)
    sb2_1 <- q_sigb2_A(ab0, bb0, b1, A1_inv)
    sh2_1 <- q_sigh2(ah0, bh0, h1)
    spsi2_1 <- q_sigpsi2(apsi0, bpsi0, Varphi1, theta0, loc_rela)
    q_theta_out <- q_theta_trans_rw(theta0, spsi2_1, Varphi1, loc_rela, kappa)
    theta1 <- q_theta_out[[1]]
    Rej_theta[i,] <- q_theta_out[[2]]
    y_hat <- q_y_SFW(VAR_ind, ENV_ind, Phi1, g1, b1, h1)

    if ( (i > burn_in) & ((i-burn_in)%%thin == 1) ){
      i1 <- (i - burn_in - 1)/thin + 1
      Y_hat[i1, ] <- y_hat
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


  Y_hat1 <- apply(Y_hat, 2, mean) + muY

  Output <- list(gsamps = gibbs_sample, yhat = Y_hat1)

  return(Output)
}






SFW_I_Z <- function(M, burn_in, thin,
                    Y, VAR, ENV, COOR, Z, Phi0,
                    g0, b0, h0, ga0,
                    se2_0, sg2_0, sb2_0, sh2_0, sga2_0, spsi2_0,
                    ae0, be0, ag0, bg0,
                    ab0, bb0, ah0, bh0,
                    aga0, bga0, apsi0,
                    bpsi0, theta0, kappa){
  muY <- mean(Y)
  Yc <- Y - mean(Y)
  N <- length(Y)
  N_para <- length(g0) + length(b0) + length(h0) +
    length(ga0) + length(se2_0) + length(sg2_0) +
    length(sb2_0) + length(sh2_0) + length(sga2_0) +
    length(spsi2_0) + length(theta0)

  M_use <- ceiling( (M - burn_in) / thin )
  gibbs_sample <- matrix(0, ncol = N_para, nrow = M_use)
  Y_hat <- matrix(0, ncol = N, nrow = M_use)
  loc_rela <- loc_relation(ENV, COOR)
  Rej_theta <- matrix(0, ncol = length(theta0), nrow = M)
  n <- length(loc_rela$loc_name)
  B <- besag_B(loc_rela)

  g_ind <- names(table(VAR))
  b_ind <- g_ind
  h_ind <- names(table(ENV))
  Z <- Z_change(Z, h_ind)
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
    h1 <- q_h_SFW_Z(Yc, VAR_ind, Phi0, Z, g1, b1, ga0, se2_0, sh2_0, loc_rela)
    ga1 <- q_ga(Z, h1, sh2_0, sga2_0)
    Varphi1 <- q_varphi(Yc, VAR_ind, g1, b1, h1, se2_0, spsi2_0, theta0, loc_rela, B)
    Psi1 <- varphi2psi(Varphi1, n, B)
    Phi1 <- psi2phi(Psi1, loc_rela, N)
    se2_1 <- q_sige2_SFW(ae0, be0, Yc, VAR_ind, ENV_ind, Phi1, g1, b1, h1)
    sg2_1 <- q_sigg2_I(ag0, bg0, g1)
    sb2_1 <- q_sigb2_I(ab0, bb0, b1)
    sh2_1 <- q_sigh2_Z(ah0, bh0, h1, ga1, Z)
    sga2_1 <- q_sigga2(aga0, bga0, ga1)
    spsi2_1 <- q_sigpsi2(apsi0, bpsi0, Varphi1, theta0, loc_rela)
    q_theta_out <- q_theta_trans_rw(theta0, spsi2_1, Varphi1, loc_rela, kappa)
    theta1 <- q_theta_out[[1]]
    Rej_theta[i,] <- q_theta_out[[2]]
    y_hat <- q_y_SFW(VAR_ind, ENV_ind, Phi1, g1, b1, h1)

    if ( (i > burn_in) & ((i-burn_in)%%thin == 1) ){
      i1 <- (i - burn_in - 1)/thin + 1
      Y_hat[i1, ] <- y_hat
      gibbs_sample[i1, ] <- c(g1,b1,h1,ga1,se2_1,sg2_1,sb2_1,sh2_1,sga2_1,spsi2_1,theta1)
    }

    g0 <- g1
    b0 <- b1
    h0 <- h1
    ga0 <- ga1
    Phi0 <- Phi1
    se2_0 <- se2_1
    sg2_0 <- sg2_1
    sb2_0 <- sb2_1
    sh2_0 <- sh2_1
    sga2_0 <- sga2_1
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
                              paste("gamma",(1:length(ga1)),sep="-"),
                              "var_e", "var_g", "var_b", "var_h", "var_ga",
                              paste("var_psi",h_ind,sep="-"),
                              paste("theta",h_ind,sep="-"))


  Y_hat1 <- apply(Y_hat, 2, mean) + muY

  Output <- list(gsamps = gibbs_sample, yhat = Y_hat1)

  return(Output)
}







SFW_A_Z <- function(M, burn_in, thin,
                    Y, VAR, ENV, COOR, Z, A, Phi0,
                    g0, b0, h0, ga0,
                    se2_0, sg2_0, sb2_0, sh2_0, sga2_0, spsi2_0,
                    ae0, be0, ag0, bg0,
                    ab0, bb0, ah0, bh0,
                    aga0, bga0, apsi0,
                    bpsi0, theta0, kappa){
  muY <- mean(Y)
  Yc <- Y - mean(Y)
  N <- length(Y)
  N_para <- length(g0) + length(b0) + length(h0) +
    length(ga0) + length(se2_0) + length(sg2_0) +
    length(sb2_0) + length(sh2_0) + length(sga2_0) +
    length(spsi2_0) + length(theta0)

  M_use <- ceiling( (M - burn_in) / thin )
  gibbs_sample <- matrix(0, ncol = N_para, nrow = M_use)
  Y_hat <- matrix(0, ncol = N, nrow = M_use)
  loc_rela <- loc_relation(ENV, COOR)
  Rej_theta <- matrix(0, ncol = length(theta0), nrow = M)
  n <- length(loc_rela$loc_name)
  B <- besag_B(loc_rela)

  g_ind <- names(table(VAR))
  b_ind <- g_ind
  h_ind <- names(table(ENV))
  Z <- Z_change(Z, h_ind)
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
    h1 <- q_h_SFW_Z(Yc, VAR_ind, Phi0, Z, g1, b1, ga0, se2_0, sh2_0, loc_rela)
    ga1 <- q_ga(Z, h1, sh2_0, sga2_0)
    Varphi1 <- q_varphi(Yc, VAR_ind, g1, b1, h1, se2_0, spsi2_0, theta0, loc_rela, B)
    Psi1 <- varphi2psi(Varphi1, n, B)
    Phi1 <- psi2phi(Psi1, loc_rela, N)
    se2_1 <- q_sige2_SFW(ae0, be0, Yc, VAR_ind, ENV_ind, Phi1, g1, b1, h1)
    sg2_1 <- q_sigg2_A(ag0, bg0, g1, A1_inv)
    sb2_1 <- q_sigb2_A(ab0, bb0, b1, A1_inv)
    sh2_1 <- q_sigh2_Z(ah0, bh0, h1, ga1, Z)
    sga2_1 <- q_sigga2(aga0, bga0, ga1)
    spsi2_1 <- q_sigpsi2(apsi0, bpsi0, Varphi1, theta0, loc_rela)
    q_theta_out <- q_theta_trans_rw(theta0, spsi2_1, Varphi1, loc_rela, kappa)
    theta1 <- q_theta_out[[1]]
    Rej_theta[i,] <- q_theta_out[[2]]
    y_hat <- q_y_SFW(VAR_ind, ENV_ind, Phi1, g1, b1, h1)

    if ( (i > burn_in) & ((i-burn_in)%%thin == 1) ){
      i1 <- (i - burn_in - 1)/thin + 1
      Y_hat[i1, ] <- y_hat
      gibbs_sample[i1, ] <- c(g1,b1,h1,ga1,se2_1,sg2_1,sb2_1,sh2_1,sga2_1,spsi2_1,theta1)
    }

    g0 <- g1
    b0 <- b1
    h0 <- h1
    ga0 <- ga1
    Phi0 <- Phi1
    se2_0 <- se2_1
    sg2_0 <- sg2_1
    sb2_0 <- sb2_1
    sh2_0 <- sh2_1
    sga2_0 <- sga2_1
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
                              paste("gamma",(1:length(ga1)),sep="-"),
                              "var_e", "var_g", "var_b", "var_h", "var_ga",
                              paste("var_psi",h_ind,sep="-"),
                              paste("theta",h_ind,sep="-"))


  Y_hat1 <- apply(Y_hat, 2, mean) + muY

  Output <- list(gsamps = gibbs_sample, yhat = Y_hat1)

  return(Output)
}








