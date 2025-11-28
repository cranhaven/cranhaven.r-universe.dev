#' MTAFT_IC: Multiple Thresholds Accelerated Failure Time Model with Information Criteria
#'
#' This function implements a method for multiple thresholds accelerated failure time (AFT) model
#' with information criteria. It estimates the subgroup-specific slope coefficients and variance
#' estimates, as well as the threshold estimates using either the "WBS" (Wild Binary Segmentation)
#' or "DP" (Dynamic Programming) algorithm.
#'
#' @param Y the censored logarithm of the failure time.
#' @param X the design matrix without the intercept.
#' @param delta the censoring indicator.
#' @param Tq the threshold values.
#' @param c0 the penalty factor c0 in the information criteria (IC), default is 0.299.
#' @param delta0 the penalty factor delta0 in the information criteria (IC), default is 2.01.
#' @param algorithm the threshold detection algorithm, either "WBS" or "DP". Default is "WBS".
#' @param dist_min the pre-specified minimal number of observations within each subgroup. Default is 50.
#' @param ncps_max the pre-specified maximum number of thresholds. Default is 4.
#' @param wbs_nintervals the number of random intervals in the WBS algorithm. Default is 200.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{params}{the subgroup-specific slope estimates and variance estimates.}
#'   \item{thres}{the threshold estimates.}
#'   \item{IC_val}{the IC values for all candidate number of thresholds.}
#' }
#'
#' @references
#' (Add relevant references here)
#'
#' @examples
#' \donttest{
#' # Generate simulated data with 500 samples and normal error distribution
#' dataset <- MTAFT_simdata(n = 500, err = "normal")
#' Y <- dataset[, 1]
#' delta <- dataset[, 2]
#' Tq <- dataset[, 3]
#' X <- dataset[, -c(1:3)]
#'
#' # Run MTAFT_IC with WBS algorithm
#' mtaft_ic_result <- MTAFT_IC(Y, X, delta, Tq, algorithm = 'WBS')
#' mtaft_ic_result$params
#' mtaft_ic_result$thres
#' mtaft_ic_result$IC_val
#' }
#' @export

MTAFT_IC <- function(Y,
                    X,
                    delta,
                    Tq,
                    c0 = 0.299,
                    delta0 = 2.01,
                    algorithm = c("WBS","DP"),
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

  if (algorithm == "DP") {
    res_est <- DP_custom_naive_R_AFT(dataset,n, g_subdat, g_param_AFT, ncps_max, dist_min)$cpt_cand
    idx <- !apply(is.na(res_est), 1, all)
    res_est <- matrix(res_est[idx, ], sum(idx), ncps_max)
  } else if (algorithm == "WBS") {
    # set.seed(123)
    lr_M <- matrix(NA, wbs_nintervals, 2)
    for (i in 1:wbs_nintervals) {
      lr_M[i, ] <- sort(sample(0:n, 2, replace = FALSE))
    }
    res_est <- WBS_custom_naive_R_AFT(dataset, n, g_subdat, g_param_AFT, ncps_max, dist_min, lr_M)$cpt_cand
  }

  ncps_max1 <- nrow(res_est)
  IC_val <- rep(NA, ncps_max1)
  p= ncol(as.matrix(X))+1

  for (k in 1:ncps_max1) {
    cps_k_subdat <- sort(res_est[k, k:1])
    cps_k <- (1:n)[cps_k_subdat]
    ID <- rep(1:(k + 1), c(cps_k, n) - c(0, cps_k))
    cost_val_k_cum <- 0
    for (j in 1:(k + 1)) {
      subdat_kj <- g_subdat(dataset, ID == j)
      param_kj <- g_param_AFT(subdat_kj)
      cost_val_k_cum <- cost_val_k_cum + param_kj
    }
    IC_val[k] <- log(cost_val_k_cum/n)+c0*p*(k+1)*((log(n))^delta0)/n
  }

  ncps <- which.min(IC_val)

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
  px= p-1
  row.name = c("cons",paste0("x",1:px),"sigma2")
  rownames(params) = row.name
  col.name = paste0("subgroup","-",1:(ncps+1))
  colnames(params) = col.name
  thres = dataset[,3][cps]

  return(list(params = params, thres=thres,IC_val=IC_val ))



}
