#' @title Prediction Function for Hierarchical Spatial Finlay-Wilkinson Model
#'
#' @description This function considers spatial adjustments.
#'
#'
#' @param Y A length-N1 numerical response vector from training set
#' @param VAR A length-N1 factor/character vector indicating the genotype information of Y
#' @param ENV A length-N1 factor/character vector indicating the field information of Y
#' @param COOR A N1 by 2 numerical matrix indicating the spatial locations of Y
#' @param VAR2 A length-N2 factor/character vector indicating the genotype information of testing set
#' @param ENV2 A length-N2 factor/character vector indicating the field information of of testing set
#' @param COOR2 A N2 by 2 numerical matrix indicating the spatial locations of testing set
#' @param save_int A logical parameter controling whether to save prediction credible intervals
#' @param kin_info A logical parameter controling if to use kinship matrix
#' @param A kinship matrix, give value only if kin_info = TRUE
#' @param inits initial values, default is given
#' @param hyper_para hyper-parameter values, default is given
#' @param M_iter Total iteration number
#' @param burn_in Burn in number
#' @param thin Thinning value
#' @param seed Random seed value
#'
#' @return  Mean prediction values and/or prediction intervals
#' @export
#'
#'@example man/examples/example_HSFWM_pred.R

HSFWM_pred <- function(Y, VAR, ENV, COOR, VAR2, ENV2, COOR2, save_int = FALSE,
                       kin_info = FALSE, A = NULL,
                       inits = NULL, hyper_para = NULL, M_iter = 5000,
                       burn_in = 3000, thin = 5, seed = NULL){

  VAR <- as.character(VAR)
  ENV <- as.character(ENV)
  VAR2 <- as.character(VAR2)
  ENV2 <- as.character(ENV2)

  set.seed(seed = seed)
  g_ind <- names(table(VAR))
  b_ind <- names(table(VAR))
  h_ind <- names(table(ENV))

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
  }

  if ( kin_info == FALSE ){
    out <- SFW_I_pred(M_iter, burn_in, thin,
                      Y, VAR, ENV, COOR, ENV2, COOR2,Phi0,
                      g0, b0, h0,
                      se2_0, sg2_0, sb2_0, sh2_0, spsi2_0,
                      ae0, be0, ag0, bg0,
                      ab0, bb0, ah0, bh0,
                      apsi0, bpsi0, theta0, kappa0)
    gsamps <- out$gsamps
    phi2hat <- out$phi2hat
  }

  if ( kin_info == TRUE ){
    out <- SFW_A_pred(M_iter, burn_in, thin,
                      Y, VAR, ENV, COOR, ENV2, COOR2, A, Phi0,
                      g0, b0, h0,
                      se2_0, sg2_0, sb2_0, sh2_0, spsi2_0,
                      ae0, be0, ag0, bg0,
                      ab0, bb0, ah0, bh0,
                      apsi0, bpsi0, theta0, kappa0)
    gsamps <- out$gsamps
    phi2hat <- out$phi2hat
  }

  muhat <- mean(Y)
  g_ind2 <- names(table(VAR2))

  if ( sum( !( g_ind2 %in% g_ind ) )==0 ){ ## no new genotypes in the testing set
    col_g <-  which(sub(pattern = "-.*", replacement = "", x = colnames(gsamps))=="g")
    col_b <-  which(sub(pattern = "-.*", replacement = "", x = colnames(gsamps))=="b")
    col_h <-  which(sub(pattern = "-.*", replacement = "", x = colnames(gsamps))=="h")
    ghat <- gsamps[,col_g]
    bhat <- gsamps[,col_b]
    hhat <- gsamps[,col_h]
    sige2hat <- gsamps[,which(colnames(gsamps) == "var_e")]

    Outputs <- SFW_pred(VAR2, ENV2, muhat, phi2hat,
                        ghat, bhat, hhat, sige2hat, save_int)
  }


  if ( sum( !( g_ind2 %in% g_ind ) )>0 ){ ## there are new genotypes in the testing set
    col_g <-  which(sub(pattern = "-.*", replacement = "", x = colnames(gsamps))=="g")
    col_b <-  which(sub(pattern = "-.*", replacement = "", x = colnames(gsamps))=="b")
    col_h <-  which(sub(pattern = "-.*", replacement = "", x = colnames(gsamps))=="h")
    ghat <- gsamps[,col_g]
    bhat <- gsamps[,col_b]
    hhat <- gsamps[,col_h]
    sige2hat <- gsamps[,which(colnames(gsamps) == "var_e")]
    sigg2hat <- gsamps[,which(colnames(gsamps) == "var_g")]
    sigb2hat <- gsamps[,which(colnames(gsamps) == "var_b")]

    Outputs <- SFW_pred_new(VAR, VAR2, ENV2, muhat, phi2hat,
                            ghat, bhat, hhat, sige2hat, sigg2hat, sigb2hat,
                            kin_info, A, save_int)
  }

  return(Outputs)

}

