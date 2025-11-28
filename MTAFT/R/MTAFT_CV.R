
WBS_custom_naive_R_AFT <- function(dataset,n, g_subdat, g_param_AFT, L, d, lr_M) {

  out <- list()
  #n <- nrow(dataset)
  M <- nrow(lr_M)
  all_seg <- matrix(NA, n, n)
  for (i in 1:M) {
    idx_temp <- rep(FALSE, n)
    idx_temp[(lr_M[i, 1] + 1):lr_M[i, 2]] <- TRUE
    subdat_temp <- g_subdat(dataset, idx_temp)
    param_temp <- try(g_param_AFT(subdat_temp),silent = TRUE)
    all_seg[lr_M[i, 1] + 1, lr_M[i, 2]] <- ifelse(is(param_temp,"try-error"),NA,param_temp)       #g_cost(subdat_temp, param_temp)
  }
  tau_hat_all <- c(0, rep(NA, L + 1))
  for (k in 1:L) {
    tau_hat_all[k + 1] <- n
    cost_cmp <- matrix(NA, k, M + 1)
    cps_cmp <- matrix(NA, k, M + 1)
    for (j in 0:(k - 1)) {
      l <- sort(tau_hat_all)[j + 1]
      r <- sort(tau_hat_all)[j + 1 + 1]
      for (i in 1:(M + 1)) {
        if (i == M + 1) {
          li <- l
          ri <- r
        } else {
          li <- lr_M[i, 1]
          ri <- lr_M[i, 2]
        }
        if (l <= li & ri <= r) {
          if ( sum(dataset[(ri):(li),2]) >= 2*d) { #
            t_lr <- rep(NA, ri - li)
            count <- 0
            for (t in li:ri) {
              if (t > li & t < ri & min(t - li, ri - t) >= d) {
                count <- count + 1
                t_lr[count] <- t
                if (is.na(all_seg[li + 1, t])) {
                  idx_temp <- rep(FALSE, n)
                  idx_temp[(li + 1):t] <- TRUE
                  subdat_temp <- g_subdat(dataset,idx_temp)
                  param_temp <- g_param_AFT(subdat_temp)
                  all_seg[li + 1, t] <- param_temp#sum(param_temp$residuals^2)
                }
                if (is.na(all_seg[t + 1, ri])) {
                  idx_temp <- rep(FALSE, n)
                  idx_temp[(t + 1):ri] <- TRUE
                  subdat_temp <- g_subdat(dataset, idx_temp)
                  param_temp <- g_param_AFT(subdat_temp)
                  all_seg[t + 1, ri] <-  param_temp#sum(param_temp$residuals^2)  #g_cost(subdat_temp, param_temp)
                }
              }
            }
            if (is.na(all_seg[li + 1, ri])) {
              idx_temp <- rep(FALSE, n)
              idx_temp[(li + 1):ri] <- TRUE
              subdat_temp <- g_subdat(dataset, idx_temp)
              param_temp <- g_param_AFT(subdat_temp)
              all_seg[li + 1, ri] <- param_temp #sum(param_temp$residuals^2)  #g_cost(subdat_temp, param_temp)
            }
            temp <- all_seg[li + 1, ri] - (all_seg[li + 1, t_lr] + all_seg[t_lr + 1, ri])
            temp_which_max <- which.max(temp)
            cost_cmp[j + 1, i] <- temp[temp_which_max]
            cps_cmp[j + 1, i] <- t_lr[temp_which_max]
          }
        }
      }
    }
    if (!all(is.na(cost_cmp))) {
      tau_hat_all[1 + k] <- cps_cmp[which.max(cost_cmp)]
      # cps_cmp[which(cost_cmp == max(cost_cmp, na.rm = TRUE), arr.ind = TRUE)]
    } else {
      break
    }
  }

  tau_hat <- tau_hat_all[!is.na(tau_hat_all)]
  tau_hat <- tau_hat[0 < tau_hat & tau_hat < n]

  cpt_cand <- matrix(NA, length(tau_hat), length(tau_hat))
  for (i in 1:length(tau_hat)) {
    cpt_cand[i, 1:i] <- sort(tau_hat[1:i])
  }

  out$cpt_cand <- cpt_cand
  out$cpt_cand_temp <- tau_hat
  return(out)
}

DP_custom_naive_R_AFT <- function(dataset,n, g_subdat, g_param_AFT, L, d) {

  out <- list()
  #n <- nrow(dataset)
  Q <- L + 1
  all_seg <- matrix(NA, n, n)
  for (i in 1:(n - d + 1)) {
    for (j in i:n) {
      if ((j >= i + d - 1) & (sum(dataset[i:j,2]) > d)) {
        idx_temp <- rep(FALSE, n)
        idx_temp[i:j] <- TRUE
        subdat_temp <- g_subdat(dataset, idx_temp)
        param_temp <- g_param_AFT(subdat_temp)
        all_seg[i, j] <- param_temp#sum(param_temp$residuals^2)  #g_cost(subdat_temp, param_temp)
      }
    }
  }

  loss <- matrix(NA, Q, n)
  loss[1, ] <- all_seg[1, ]
  V <- matrix(NA, Q, n)
  for (q in 2:Q) {
    for (j in (q * d):n) {
      v <- ((q - 1) * d):(j - d)
      loss_temp <- loss[q - 1, v] + all_seg[v + 1, j]
      if(all(is.na(loss_temp))) next()
      v_min <- which.min(loss_temp)
      loss[q, j] <- loss_temp[v_min]
      V[q, j] <- v_min + (q - 1) * d - 1
    }
  }

  cps <- matrix(NA, Q, L)
  cps[, 1] = V[, n]
  if (Q >= 3) {
    for (q in 3:Q) {
      for (i in 2:(q - 1)) {
        cps[q, i] <- V[q - (i - 1), cps[q, i - 1]]
      }
    }
  }

  cpt_cand <- matrix(cps[2:Q, 1:L], L, L)
  for (i in 1:L) {
    cpt_cand[i, 1:i] <- cpt_cand[i, i:1]
  }

  out$cpt_cand <- cpt_cand

  return(out)
}


g_subdat <- function(dat, indices) {
  matrix(dat[indices, ], sum(indices), ncol(dat))
}

g_param_AFT <- function(dat) {
  y <- dat[, 1]
  delta <- dat[,2]
  Tq <- dat[,3]
  x <- as.matrix(dat[,-c(1:3)])
  o <- order(y)

  y1 <- y[o]
  Tq1 <- Tq[o]
  x1 <- x[o, ]
  delta1 <- delta[o]
  n = length(y)

  Wn1 <- delta1
  Wn1[1] <- delta1[1]/n
  tt <- 1
  for (i in 2:n) {
    j <- i - 1
    tt <- tt * ((n - j)/(n - j + 1))^delta1[j]
    Wn1[i] <- delta1[i]/(n - i + 1) * tt
  }

  W <- sqrt(Wn1)
  y2 <- W * y1
  x2 <- W * x1
  X <- cbind(W,x2)
  lmr <- lm(y2 ~ W+x2 - 1)

  loss <- sum(lmr$residuals^2)*n

  return(loss)
}

g_coef_AFT <- function(dat) {
  y <- dat[, 1]
  delta <- dat[,2]
  Tq <- dat[,3]
  x <- as.matrix(dat[,-c(1:3)])
  o <- order(y)

  y1 <- y[o]
  Tq1 <- Tq[o]
  x1 <- x[o, ]
  delta1 <- delta[o]
  n = length(y)

  Wn1 <- delta1
  Wn1[1] <- delta1[1]/n
  tt <- 1
  for (i in 2:n) {
    j <- i - 1
    tt <- tt * ((n - j)/(n - j + 1))^delta1[j]
    Wn1[i] <- delta1[i]/(n - i + 1) * tt
  }

  W <- sqrt(Wn1)
  y2 <- W * y1
  x2 <- W * x1
  X <- cbind(W,x2)
  lmr <- lm(y2 ~ W+x2 - 1)

  return(lmr)
}

g_cost_AFT <- function(dat,coef){
  y <- dat[, 1]
  delta <- dat[,2]
  Tq <- dat[,3]
  x <- as.matrix(dat[,-c(1:3)])
  o <- order(y)

  y1 <- y[o]
  Tq1 <- Tq[o]
  x1 <- x[o, ]
  delta1 <- delta[o]
  n = length(y)

  Wn1 <- delta1
  Wn1[1] <- delta1[1]/n
  tt <- 1
  for (i in 2:n) {
    j <- i - 1
    tt <- tt * ((n - j)/(n - j + 1))^delta1[j]
    Wn1[i] <- delta1[i]/(n - i + 1) * tt
  }

  W <- sqrt(Wn1)
  y2 <- W * y1
  x2 <- W * x1

  sum((y2 - cbind(W,x2) %*% coef)^2)*n
}


COPS_AFT <- function(dataset,n,indices,algorithm, dist_min, ncps_max, wbs_nintervals){

  subdat <- g_subdat(dataset, indices)
  n_subdat <- sum(indices)


  if (algorithm == "DP") {
    res <- DP_custom_naive_R_AFT(subdat,n_subdat, g_subdat, g_param_AFT, ncps_max, dist_min)$cpt_cand
    idx <- !apply(is.na(res), 1, all)
    res <- matrix(res[idx, ], sum(idx), ncps_max)

  } else if (algorithm == "WBS") {
    # set.seed(123)
    lr_M <- matrix(NA, wbs_nintervals, 2)
    for (i in 1:wbs_nintervals) {
      lr_M[i, ] <- sort(sample(0:n_subdat, 2, replace = FALSE))
    }
    res <- WBS_custom_naive_R_AFT(subdat, n_subdat, g_subdat, g_param_AFT, ncps_max, dist_min, lr_M)$cpt_cand
  }
  cost_val <- rep(NA, ncps_max)

  ncps_max1 <- nrow(res)
  for (k in 1:ncps_max1) {
    cps_k_subdat <- sort(res[k, k:1])
    #cps_ak <- (1:n)[2*cps_k_subdat]
    cps_k <- (1:n)[indices][cps_k_subdat]
    ID <- rep(1:(k + 1), c(cps_k, n) - c(0, cps_k))
    cost_val_k_cum <- 0
    for (j in 1:(k + 1)) {
      subdat_kj <- g_subdat(dataset, ID == j & indices) #
      subdat_val_kj <- g_subdat(dataset, ID == j  &(!indices)) #
      paramf <- g_coef_AFT(subdat_kj)
      coef = paramf$coefficients
      cost_val_k_cum <- cost_val_k_cum + g_cost_AFT(subdat_val_kj,coef)  #g_cost(subdat_val_kj, param_kj)
      #cost_val_k_cum <- cost_val_k_cum  + g_Score_AFT(subdat_val_kj,mean_kj,coef)
    }
    cost_val[k] <- cost_val_k_cum
  }

  return(cost_val)

}


#' MTAFT_CV: Cross-Validation for Multiple Thresholds Accelerated Failure Time Model
#'
#' This function implements a cross-validation method for the multiple thresholds accelerated
#' failure time (AFT) model using either the "WBS" (Wild Binary Segmentation) or "DP"
#' (Dynamic Programming) algorithm. It determines the optimal number of thresholds by
#' evaluating the cross-validation (CV) values.
#'
#' @param Y the censored logarithm of the failure time.
#' @param X the design matrix without the intercept.
#' @param delta the censoring indicator.
#' @param Tq the threshold values.
#' @param algorithm the threshold detection algorithm, either "WBS" or "DP".
#' @param dist_min the pre-specified minimal number of observations within each subgroup. Default is 50.
#' @param ncps_max the pre-specified maximum number of thresholds. Default is 4.
#' @param wbs_nintervals the number of random intervals in the WBS algorithm. Default is 200.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{params}{the subgroup-specific slope estimates and variance estimates.}
#'   \item{thres}{the threshold estimates.}
#'   \item{CV_vals}{the CV values for all candidate number of thresholds.}
#' }
#'
#' @examples
#' # Generate simulated data with 500 samples and normal error distribution
#' dataset <- MTAFT_simdata(n = 500, err = "normal")
#' \donttest{
#' Y <- dataset[, 1]
#' delta <- dataset[, 2]
#' Tq <- dataset[, 3]
#' X <- dataset[, -c(1:3)]
#'
#' # Run mAFT_CV with WBS algorithm
#' maft_cv_result <- MTAFT_CV(Y, X, delta, Tq, algorithm = "WBS")
#' maft_cv_result$params
#' maft_cv_result$thres
#' maft_cv_result$CV_vals
#' }
#' @export
MTAFT_CV <- function(Y,
                    X,
                    delta,
                    Tq,
                     algorithm,
                     dist_min=50,
                     ncps_max=4,
                     wbs_nintervals =200 )
{

  dataset = cbind(Y,delta,Tq,X)
  ord = order(dataset[,3])
  dataset = dataset[ord,]
  n <- nrow(dataset)
  S_vals <- 1:ncps_max
  idx <- 1:n
  is_O <- idx %% 2 == 1
  n_O <- sum(is_O)
  idx_O <- vector("list", 2)
  idx_O[[1]] <- is_O
  idx_O[[2]] <- !is_O
  SC_vals <- matrix(NA, 2, length(S_vals))

  n1 = sum(delta)

  if(dist_min > ceiling(n1/ncps_max)){
    stop("The minimal number of observations between any two thresholds is too large")
  }
  if(dist_min < ceiling(sqrt(n))){
    stop("The minimal number of observations between any two thresholds cannot be too small")
  }

  dist_min_sub = ceiling(dist_min/2)
  for (i in 1:2) {
    res_i <- COPS_AFT(dataset,n,idx_O[[i]],algorithm, dist_min_sub, ncps_max, wbs_nintervals)
    SC_vals[i, ] <- res_i
  }
  ncps <- which.min(apply(SC_vals, 2, sum))

  ##Refit

  if (algorithm == "DP") {
    res_refit <- DP_custom_naive_R_AFT(dataset,n, g_subdat, g_param_AFT, ncps, dist_min)$cpt_cand
    idx <- !apply(is.na(res_refit), 1, all)
    res_refit <- matrix(res_refit[idx, ], sum(idx), ncps)

  } else if (algorithm == "WBS") {
    # set.seed(123)
    lr_M <- matrix(NA, wbs_nintervals, 2)
    for (i in 1:wbs_nintervals) {
      lr_M[i, ] <- sort(sample(0:n, 2, replace = FALSE))
    }
    res_refit <- WBS_custom_naive_R_AFT(dataset, n, g_subdat, g_param_AFT, ncps, dist_min, lr_M)$cpt_cand
  }

  cps <- sort(res_refit[ncps, 1:ncps])

  nseg <- length(cps) + 1
  params <- c()
  ID <- rep(1:(length(cps) + 1), c(cps, n) - c(0, cps))

  for (j in 1:(length(cps) + 1)) {
    subdat_j <- g_subdat(dataset, ID == j)
    params_cut <- g_coef_AFT(subdat_j)
    nj <- nrow(subdat_j)
    params <- cbind(params,c(params_cut$coefficients,sum(params_cut$residuals^2)))
  }
  px= ncol(as.matrix(X))
  row.name = c("cons",paste0("x",1:px),"sigma2")
  rownames(params) = row.name
  col.name = paste0("subgroup","-",1:(ncps+1))
  colnames(params) = col.name
  thres = dataset[,3][cps]

  return(list(params = params, thres=thres,CV_vals=SC_vals ))

}







