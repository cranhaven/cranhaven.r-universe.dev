EST <- function(dataset, n, g_subdat, g_param, g_cost, algorithm, dist_min, ncps_max, pelt_pen_val, pelt_K, wbs_nintervals, model, g_smry, easy_cost, param.opt) {

  if (is.null(g_smry)) {

    if (algorithm == "SN") {
      res <- SN_custom_naive_R(dataset, n, g_subdat, g_param, g_cost, ncps_max, dist_min, param.opt)$cpt_cand
    } else if (algorithm == "BS") {
      res <- BS_custom_naive_R(dataset, n, g_subdat, g_param, g_cost, ncps_max, dist_min, param.opt)$cpt_cand
    } else if (algorithm == "WBS") {
      lr_M <- matrix(NA, wbs_nintervals, 2)
      for (i in 1:wbs_nintervals) {
        lr_M[i, ] <- sort(sample(0:n, 2, replace = FALSE))
      }
      res <- WBS_custom_naive_R(dataset, n, g_subdat, g_param, g_cost, ncps_max, dist_min, lr_M, param.opt)$cpt_cand
    } else if (algorithm == "PELT") {
      res <- PELT_custom_naive_R(dataset, n, g_subdat, g_param, g_cost, pelt_pen_val, dist_min, pelt_K, param.opt)$cpt_cand
      temp_n_row <- length(res[res[, 1] > 0, 1])
      res <- res[1:temp_n_row, ]
    } else {
      stop("Not yet supported change searching algorithm!")
    }

  } else {

    dat_smry <- g_smry(dataset, param.opt)

    if (is.null(easy_cost)) {

      if (algorithm == "SN") {
        res <- SN(dat_smry, n, ncps_max, dist_min, model)$cpt_cand
      } else if (algorithm == "BS") {
        res <- BS(dat_smry, n, ncps_max, dist_min, model)$cpt_cand
      } else if (algorithm == "WBS") {
        lr_M <- matrix(NA, wbs_nintervals, 2)
        for (i in 1:wbs_nintervals) {
          lr_M[i, ] <- sort(sample(0:n, 2, replace = FALSE))
        }
        res <- WBS(dat_smry, n, ncps_max, dist_min, lr_M, model)$cpt_cand
      } else if (algorithm == "PELT") {
        res <- PELT(dat_smry, n, pelt_pen_val, dist_min, pelt_K, model)$cpt_cand
        temp_n_row <- length(res[res[, 1] > 0, 1])
        res <- res[1:temp_n_row, ]
      } else {
        stop("Not yet supported change searching algorithm!")
      }

    } else {

      if (algorithm == "SN") {
        res <- SN_custom_R(dat_smry, n, ncps_max, dist_min, easy_cost)$cpt_cand
      } else if (algorithm == "BS") {
        res <- BS_custom_R(dat_smry, n, ncps_max, dist_min, easy_cost)$cpt_cand
      } else if (algorithm == "WBS") {
        lr_M <- matrix(NA, wbs_nintervals, 2)
        for (i in 1:wbs_nintervals) {
          lr_M[i, ] <- sort(sample(0:n, 2, replace = FALSE))
        }
        res <- WBS_custom_R(dat_smry, n, ncps_max, dist_min, lr_M, easy_cost)$cpt_cand
      } else if (algorithm == "PELT") {
        res <- PELT_custom_R(dat_smry, n, pelt_pen_val, dist_min, pelt_K, easy_cost)$cpt_cand
        temp_n_row <- length(res[res[, 1] > 0, 1])
        res <- res[1:temp_n_row, ]
      } else {
        stop("Not yet supported change searching algorithm!")
      }

    }

  }

  if (algorithm == "PELT") {
    res <- matrix(res, temp_n_row, length(pelt_pen_val))
  }

  if (algorithm == "SN") {
    idx <- !apply(is.na(res), 1, all)
    res <- matrix(res[idx, ], sum(idx), ncps_max)
  }

  return(res)
}

#' @importFrom stats approx
COPS <- function(dataset, n, indices, g_subdat, g_param, g_cost, algorithm, dist_min, ncps_max, pelt_pen_val, pelt_K, wbs_nintervals, model, g_smry, easy_cost, param.opt) {

  res <- list()
  subdat <- g_subdat(dataset, indices)
  n_subdat <- sum(indices)
  dist_min_subdata <- max(floor(dist_min / 2), 1)
  res_est <- EST(subdat, n_subdat, g_subdat, g_param, g_cost, algorithm, dist_min_subdata, ncps_max, pelt_pen_val, pelt_K, wbs_nintervals, model, g_smry, easy_cost, param.opt)
  cost_val <- rep(NA, ncps_max)
  if (algorithm %in% c("SN", "BS", "WBS")) {
    ncps_max1 <- nrow(res_est)
    for (k in 1:ncps_max1) {
      cps_k_subdat <- sort(res_est[k, k:1])
      cps_k <- (1:n)[indices][cps_k_subdat]
      ID <- rep(1:(k + 1), c(cps_k, n) - c(0, cps_k))
      cost_val_k_cum <- 0
      for (j in 1:(k + 1)) {
        subdat_kj <- g_subdat(dataset, ID == j & indices)
        param_kj <- g_param(subdat_kj, param.opt)
        subdat_val_kj <- g_subdat(dataset, ID == j & (!indices))
        cost_val_k_cum <- cost_val_k_cum + g_cost(subdat_val_kj, param_kj)
      }
      cost_val[k] <- cost_val_k_cum
    }
  } else if (algorithm == "PELT") {
    pelt_ncps <- rep(NA, length(pelt_pen_val))
    pelt_cost <- rep(NA, length(pelt_pen_val))
    for (k in 1:length(pelt_pen_val)) {
      cps_k_subdat <- res_est[res_est[, k] > 0, k]
      if (length(cps_k_subdat) == 0) {
        stop(paste0("No change-point is detected as the value of penalty ", pelt_pen_val[k], " is too large!"))
      }
      cps_k_subdat <- cps_k_subdat[length(cps_k_subdat):1]
      cps_k <- (1:n)[indices][cps_k_subdat]
      pelt_ncps[k] <- length(cps_k)
      ID <- rep(1:(pelt_ncps[k] + 1), c(cps_k, n) - c(0, cps_k))
      pelt_cost_k_cum <- 0
      for (j in 1:(pelt_ncps[k] + 1)) {
        subdat_kj <- g_subdat(dataset, ID == j & indices)
        param_kj <- g_param(subdat_kj, param.opt)
        subdat_val_kj <- g_subdat(dataset, ID == j & (!indices))
        pelt_cost_k_cum <- pelt_cost_k_cum + g_cost(subdat_val_kj, param_kj)
      }
      pelt_cost[k] <- pelt_cost_k_cum
    }
    ans <- unique(cbind(pelt_ncps, pelt_cost))
    ans <- ans[ans[, 1] <= ncps_max, ]
    n_unique <- nrow(ans)
    if (is.null(n_unique)) {
      stop("The values of the penalty may yield the same change-points!")
    }
    ans <- matrix(ans, n_unique, 2)
    if (n_unique >= 2) {
      cost_val <- approx(ans[, 1], ans[, 2], 1:ncps_max, method = "linear")$y
    } else {
      stop("The values of the penalty may yield the same change-points!")
    }
  } else {
    stop("Not yet supported change searching algorithm!")
  }
  res$cost_val <- cost_val
  return(res)
}

#' Detecting changes in uers-customized models
#'
#' @param dataset an \code{ANY} object that could be a vector, matrix, tensor, list, etc.
#' @param n an integer indicating the sample size of the data \code{dataset}.
#' @param g_subdat a customized \proglang{R} function of two arguments \code{dat} and \code{indices}, which extracts a subset of data \code{dat} according to a collection of time indices \code{indices}. The returned object inherits the class from that of \code{dataset}. The argument \code{dat} inherits the class from that of \code{dataset}, and the argument \code{indices} is a logical vector with \code{TRUE}s indicating extracted indices.
#' @param param.opt an \code{ANY} object specifying additional constant parameters needed for parameter estimation or cost evaluation beyond unknown parameters.
#' @param g_param a customized R function of two arguments \code{dat} (cf. \code{dat} of \code{g\_subdat}) and \code{param.opt} (cf. \code{param.opt} of \code{cpss.custom}), which returns estimated parameters based on the data segment \code{dat}. It could return a numeric value, vector, matrix, list, etc.
#' @param g_cost a customized \proglang{R} function of two arguments \code{dat} (cf. \code{dat} of \code{g\_subdat}) and \code{param}, which returns a numeric value of the associated cost for data segment \code{dat} with parameters \code{param}. The argument \code{param} inherits the class from that of the returned object of \code{g\_param}.
#' @param algorithm a character string specifying the change-point searching algorithm, one of the following choices: "SN" (segment neighborhood), "BS" (binary segmentation), "WBS" (wild binary segmentation) and "PELT" (pruned exact linear time) algorithms.
#' @param dist_min an integer specifying minimum searching distance (length of feasible segments).
#' @param ncps_max an integer specifying an upper bound of the number of true change-points.
#' @param pelt_pen_val a numeric vector specifying candidate values of the penalty only if \code{algorithm = "PELT"}.
#' @param pelt_K a numeric value for pruning adjustment only if \code{algorithm = "PELT"}. It is usually taken to be 0 if the negative log-likelihood is used as a cost, see Killick et al. (2012).
#' @param wbs_nintervals an integer specifying the number of random intervals drawn only if \code{algorithm = "WBS"}, see Fryzlewicz (2014).
#' @param criterion a character string specifying the model selection criterion, "CV" ("cross-validation") or "MS" ("multiple-splitting").
#' @param times an integer specifying how many times of sample-splitting should be performed; It should be 2 if \code{criterion = "CV"}.
#' @param model a character string indicating the considered change model.
#' @param g_smry a customized R function of two arguments \code{dataset} (cf. \code{dataset} of \code{cpss.custom}) and \code{param.opt} (cf. \code{param.opt} of \code{cpss.custom}), which calculates the summary statistics that will be used for cost evaluation. The returned object is a list.
#' @param easy_cost a customized R function of three arguments \code{data_smry}, \code{s} and \code{e}, which evaluates the value of the cost for a date segment form observed time point $s$ to $e$. The argument \code{data_smry} inherits the class from that of the returned object of \code{g_smry}.
#' @return \code{cpss.custom} returns an object of an \proglang{S4} class, called "\code{cpss}", which collects data and information required for further change-point analyses and summaries.
#' \describe{
#'   \item{\code{dat}}{data set}
#'   \item{\code{mdl}}{considered change-point model}
#'   \item{\code{algo}}{change-point searching algorithm}
#'   \item{\code{algo_param_dim}}{user-specified upper bound of the number of true change-points if \code{algorithm = "SN"/"BS"/"WBS"}, or user-specified candidate values of the penalty only if \code{algorithm = "PELT"}}
#'   \item{\code{SC}}{model selection criterion}
#'   \item{\code{ncps}}{estimated number of change-points}
#'   \item{\code{pelt_pen}}{selected value of the penalty only if \code{algorithm = "PELT"}}
#'   \item{\code{cps}}{a vector of estimated locations of change-points}
#'   \item{\code{params}}{a list object, each member is a list containing estimated parameters in the associated data segment}
#'   \item{\code{S_vals}}{a numeric vector of candidate model dimensions in terms of a sequence of numbers of change-points or values of the penalty}
#'   \item{\code{SC_vals}}{a numeric matrix, each column records the values of the  criterion based on the validation data split under the corresponding model dimension (\code{S_vals}), and each row represents a splitting at each time}
#' }
#' @export
#'
#' @references
#' Killick, R., Fearnhead, P., and Eckley, I. A. (2012). Optimal Detection of Changepoints With a Linear Computational Cost. Journal of the American Statistical Association, 107(500): 1590–1598.
#'
#' Fryzlewicz, P. (2014). Wild binary segmentation for multiple change-point detection. The Annals of Statistics, 42(6): 2243–2281.
#' @examples
#' \donttest{
#' library("cpss")
#' g_subdat_l1 <- function(dat, indices) {
#'   dat[indices]
#' }
#' g_param_l1 <- function(dat, param.opt = NULL) {
#'   return(median(dat))
#' }
#' g_cost_l1 <- function(dat, param) {
#'   return(sum(abs(dat - param)))
#' }
#' res <- cpss.custom(
#'   dataset = well, n = length(well),
#'   g_subdat = g_subdat_l1, g_param = g_param_l1, g_cost = g_cost_l1,
#'   ncps_max = 11
#' )
#' summary(res)
#' plot(well)
#' abline(v = res@cps, col = "red")
#' }
#' @importFrom methods new
cpss.custom <- function(dataset, n, g_subdat, g_param, g_cost, algorithm = "BS", dist_min = floor(log(n)), ncps_max = ceiling(n^0.4), pelt_pen_val = NULL, pelt_K = 0, wbs_nintervals = 500, criterion = "CV", times = 2, model = NULL, g_smry = NULL, easy_cost = NULL, param.opt = NULL) {

  out <- new("cpss")
  out@dat <- dataset
  out@mdl <- ifelse(is.null(model), "custom", model)
  if (!(out@mdl %in% c("mean", "var", "meanvar", "glm", "lm", "em"))) {
    out@mdl <- "custom"
  }
  out@algo <- algorithm
  if (out@algo %in% c("SN", "BS", "WBS")) {
    out@algo_param_dim <- ncps_max
  } else if (out@algo == "PELT") {
    out@algo_param_dim <- pelt_pen_val
  } else {
    stop("Not yet supported change searching algorithm!")
  }
  out@SC <- criterion
  out@S_vals <- 1:ncps_max

  # OPS
  idx <- 1:n
  is_O <- idx %% 2 == 1
  n_O <- sum(is_O)
  if (out@SC == "CV" & times > 2) {
    stop("Argument \"times\" should be 2 if Argument \"criterion\" is set to be \"CV\"!")
  }
  idx_O <- vector("list", times)
  if (out@SC == "CV") {
    idx_O[[1]] <- is_O
    idx_O[[2]] <- !is_O
  } else if (out@SC == "MS") {
    for (i in 1:times) {
      is_O_sel <- sample(c(TRUE, FALSE), n_O, replace = TRUE)
      is_O_MS <- c(rbind(is_O_sel, (!is_O_sel)))[1:n]
      idx_O[[i]] <- is_O_MS
    }
  } else {
    stop("Unsupported model slection criterion!")
  }

  # searching and selection
  out@SC_vals <- matrix(NA, times, length(out@S_vals))
  for (i in 1:times) {
    res_i <- COPS(out@dat, n, idx_O[[i]], g_subdat, g_param, g_cost, out@algo, dist_min, ncps_max, pelt_pen_val, pelt_K, wbs_nintervals, out@mdl, g_smry, easy_cost, param.opt)
    out@SC_vals[i, ] <- res_i$cost_val
  }
  out@ncps <- which.min(apply(out@SC_vals, 2, sum))

  # refit
  res_refit <- EST(out@dat, n, g_subdat, g_param, g_cost, out@algo, dist_min, out@ncps, pelt_pen_val, pelt_K, wbs_nintervals, out@mdl, g_smry, easy_cost, param.opt)
  if (out@algo %in% c("SN", "BS", "WBS")) {
    out@cps <- sort(res_refit[out@ncps, 1:out@ncps])
  } else if (out@algo == "PELT") {
    pelt_idx <- max(which(apply(res_refit > 0, 2, sum) >= out@ncps))
    out@pelt_pen <- pelt_pen_val[pelt_idx]
    out@cps <- sort(res_refit[res_refit[, pelt_idx] > 0, pelt_idx])
  } else {
    stop("Not yet supported change searching algorithm!")
  }
  ID <- rep(1:(length(out@cps) + 1), c(out@cps, n) - c(0, out@cps))
  for (j in 1:(length(out@cps) + 1)) {
    subdat_j <- g_subdat(dataset, ID == j)
    out@params[[j]] <- g_param(subdat_j, param.opt)
  }

  update_inputs <- list()
  update_inputs$n <- n
  update_inputs$g_subdat <- g_subdat
  update_inputs$g_param <- g_param
  update_inputs$g_cost <- g_cost
  update_inputs$dist_min <- dist_min
  update_inputs$ncps_max <- ncps_max
  update_inputs$pelt_pen_val <- pelt_pen_val
  update_inputs$pelt_K <- pelt_K
  update_inputs$wbs_nintervals <- wbs_nintervals
  update_inputs$g_smry <- g_smry
  update_inputs$easy_cost <- easy_cost
  update_inputs$param.opt <- param.opt
  out@update_inputs <- update_inputs

  if (out@mdl == "custom") {
    out@call <- list(call = match.call())
  }

  return(out)
}
