#' @title Estimation Function for Hierarchical Spatial Finlay-Wilkinson Model
#'
#' @description This function considers spatial adjustments.
#'
#' @param Y A length-N numerical response vector
#' @param VAR A length-N factor/character vector indicating the genotype information of Y
#' @param ENV A length-N factor/character vector indicating the field information of Y
#' @param COOR A N by 2 numerical matrix indicating the spatial locations of Y
#' @param kin_info A logical parameter controling if to use kinship matrix
#' @param A kinship matrix, give value only if kin_info = TRUE
#' @param env_info A logical parameter controling whether to use environmental covariates
#' @param Z environmental covariates matrix with rownames = field names, give value only if env_info = TRUE
#' @param inits initial values, default is given
#' @param hyper_para hyper-parameter values, default is given
#' @param M_iter Total iteration number
#' @param burn_in Burn in number
#' @param thin Thinning value
#' @param save_chain A logical parameter controling whether to save MCMC chain: 'Chains.rds' in current working directory
#' @param seed Random seed value
#'
#' @return Mean estimates and RMSE value
#' @export
#'
#'@example man/examples/example_HSFWM_est.R
#'

HSFWM_est <- function(Y, VAR, ENV, COOR, kin_info = FALSE, A = NULL, env_info = FALSE, Z = NULL,
                      inits = NULL, hyper_para = NULL, M_iter = 5000,
                      burn_in = 3000, thin = 5, save_chain = FALSE,
                      seed = NULL){

  VAR <- as.character(VAR)
  ENV <- as.character(ENV)

  set.seed(seed = seed)
  g_ind <- names(table(VAR))
  b_ind <- names(table(VAR))
  h_ind <- names(table(ENV))

  if (env_info == TRUE){
    n_z <- dim(Z)[2]
    Z <- as.matrix(Z)
  }

  if ( is.null(inits) == TRUE){
    Phi0 <- rep(0,length(Y))
    g0 <- rep(0, length(g_ind))
    names(g0) <- g_ind
    b0 <- rep(0, length(b_ind))
    names(b0) <- b_ind
    h0 <- rep(0, length(h_ind))
    names(h0) <- h_ind
    se2_0 <- 0.3 * var(Y)
    sg2_0 <- 0.2 * var(Y)
    sb2_0 <- 1
    sh2_0 <- 0.3 * var(Y)
    spsi2_0 <- rep(0.1*var(Y), length(h0))
    theta0 <- 1/2*rep(1,length(h0))

    if (env_info == TRUE){
      ga0 <- rep(0, n_z)
      sga2_0 <-  0.1* sh2_0
    }
  } else {
    Phi0 <- inits$Phi0
    g0 <- inits$g0
    b0 <- inits$b0
    h0 <- inits$h0
    se2_0 <- inits$se2_0
    sg2_0 <- inits$sg2_0
    sb2_0 <- inits$sb2_0
    sh2_0 <- inits$sh2_0
    spsi2_0 <- inits$spsi2_0
    theta0 <- inits$theta0
    if (env_info == TRUE){
      ga0 <- inits$ga0
      sga2_0 <-  inits$sga2_0
    }
  }


  if ( is.null(hyper_para) == TRUE){
    be0 <- 0.05*var(Y)
    bg0 <- 0.05*var(Y)
    bb0 <- 0.05
    bh0 <- 0.05*var(Y)
    bpsi0 <- 0.05*rep(1,length(h0))*var(Y)
    ae0 <- 1
    ag0 <- 1
    ab0 <- 1
    ah0 <- 1
    apsi0 <- 1*rep(1,length(h0))
    kappa0 <- rep(1,length(theta0))
    if (env_info == TRUE){
      bga0 <- 0.05 * var(Y)
      aga0 <- 5/2
    }
  } else {
    be0 <- hyper_para$be0
    bg0 <- hyper_para$bg0
    bb0 <- hyper_para$bb0
    bh0 <- hyper_para$bh0
    bpsi0 <- hyper_para$bpsi0
    ae0 <- hyper_para$ae0
    ag0 <- hyper_para$ag0
    ab0 <- hyper_para$ab0
    ah0 <- hyper_para$ah0
    apsi0 <- hyper_para$apsi0
    kappa0 <- hyper_para$kappa0
    if (env_info == TRUE){
      bga0 <- hyper_para$bga0
      aga0 <- hyper_para$aga0
    }
  }


  if ( (kin_info == FALSE) & (env_info == FALSE) ){
    out <- SFW_I(M_iter, burn_in, thin,
                 Y, VAR, ENV, COOR, Phi0,
                 g0, b0, h0,
                 se2_0, sg2_0, sb2_0, sh2_0, spsi2_0,
                 ae0, be0, ag0, bg0,
                 ab0, bb0, ah0, bh0,
                 apsi0, bpsi0, theta0, kappa0)

    yhat <- out$yhat
    gsamps <- out$gsamps
  }


  if ( (kin_info == TRUE) & (env_info == FALSE) ){
    out <- SFW_A(M_iter, burn_in, thin,
                 Y, VAR, ENV, COOR, A, Phi0,
                 g0, b0, h0,
                 se2_0, sg2_0, sb2_0, sh2_0, spsi2_0,
                 ae0, be0, ag0, bg0,
                 ab0, bb0, ah0, bh0,
                 apsi0, bpsi0, theta0, kappa0)

    yhat <- out$yhat
    gsamps <- out$gsamps
  }


  if ( (kin_info == FALSE) & (env_info == TRUE) ){
    out <- SFW_I_Z(M_iter, burn_in, thin,
                   Y, VAR, ENV, COOR, Z, Phi0,
                   g0, b0, h0, ga0,
                   se2_0, sg2_0, sb2_0, sh2_0, sga2_0, spsi2_0,
                   ae0, be0, ag0, bg0,
                   ab0, bb0, ah0, bh0,
                   aga0, bga0, apsi0,
                   bpsi0, theta0, kappa0)

    yhat <- out$yhat
    gsamps <- out$gsamps
  }


  if ( (kin_info == TRUE) & (env_info == TRUE) ){
    out <- SFW_A_Z(M_iter, burn_in, thin,
                   Y, VAR, ENV, COOR, Z, A, Phi0,
                   g0, b0, h0, ga0,
                   se2_0, sg2_0, sb2_0, sh2_0, sga2_0, spsi2_0,
                   ae0, be0, ag0, bg0,
                   ab0, bb0, ah0, bh0,
                   aga0, bga0, apsi0,
                   bpsi0, theta0, kappa0)

    yhat <- out$yhat
    gsamps <- out$gsamps
  }


  mu <- mean(Y)
  col_g <-  which(sub(pattern = "-.*", replacement = "", x = colnames(gsamps))=="g")
  col_b <-  which(sub(pattern = "-.*", replacement = "", x = colnames(gsamps))=="b")
  col_h <-  which(sub(pattern = "-.*", replacement = "", x = colnames(gsamps))=="h")
  col_var_psi <- which(sub(pattern = "-.*", replacement = "", x = colnames(gsamps))=="var_psi")
  col_theta <- which(sub(pattern = "-.*", replacement = "", x = colnames(gsamps))=="theta")
  g <- apply(gsamps[,col_g], 2, mean)
  names(g) <- g_ind
  b <- apply(gsamps[,col_b], 2, mean)
  names(b) <- b_ind
  h <- apply(gsamps[,col_h], 2, mean)
  names(h) <- h_ind
  var_e <- exp( mean( log( gsamps[, which(colnames(gsamps) == "var_e") ] ) ) )
  var_g <- exp( mean( log( gsamps[, which(colnames(gsamps) == "var_g") ] ) ) )
  var_b <- exp( mean( log( gsamps[, which(colnames(gsamps) == "var_b") ] ) ) )
  var_h <- exp( mean( log( gsamps[, which(colnames(gsamps) == "var_h") ] ) ) )

  var_psi <- exp( apply( log(gsamps[,col_var_psi]), 2, mean ) )
  names(var_psi) <- h_ind
  theta <- exp( apply( log(gsamps[,col_theta]), 2, mean ) )
  names(theta) <- h_ind

  if (env_info == TRUE){
    col_gamma <-  which(sub(pattern = "-.*", replacement = "", x = colnames(gsamps))=="gamma")
    if (n_z==1){
      gamma <- mean(gsamps[,col_gamma])
    } else {
      gamma <- apply(gsamps[,col_gamma], 2, mean)
    }
    col_var_ga <-  which(sub(pattern = "-.*", replacement = "", x = colnames(gsamps))=="var_ga")
    var_ga <- exp( mean( log( gsamps[,col_var_ga] ) ) )

  }


  if (env_info == FALSE){
    Outputs <- list(
      mu = mu, g = g, b = b, h = h, var_e = var_e, var_g = var_g,
      var_b = var_b, var_h = var_h, var_psi = var_psi, theta = theta,
      yhat = yhat,
      RMSE = sqrt(mean( (Y - yhat)^2 ))
    )

  } else {
    Outputs <- list(
      mu = mu, g = g, b = b, h = h, var_e = var_e, var_g = var_g,
      var_b = var_b, var_h = var_h, var_psi = var_psi, theta = theta,
      gamma = gamma,
      var_gamma = var_ga,
      yhat = yhat,
      RMSE = sqrt(mean( (Y - yhat)^2 ))
    )

  }


  if (save_chain == TRUE){
    saveRDS(gsamps, file = "Chains.rds")
  }

  return(Outputs)


}
