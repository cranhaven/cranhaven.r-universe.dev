SN_custom_R <- function(dat_smry, n, L, d, get_cost) {

  out <- list()

  Q <- L + 1

  all_seg <- matrix(NA, n, n)
  for (i in 1:(n - d + 1)) {
    for (j in i:n) {
      if (j >= i + d - 1) {
        all_seg[i, j] <- get_cost(dat_smry, i, j)
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

BS_custom_R <- function(dat_smry, n, L, d, get_cost) {

  out <- list()

  all_seg <- matrix(NA, n, n)
  all_seg[1, n] <- get_cost(dat_smry, 1, n)
  tau_hat_all <- c(0, rep(NA, L + 1))
  for (k in 1:L) {
    tau_hat_all[k + 1] <- n
    cost_cmp <- rep(NA, k)
    cps_cmp <- rep(NA, k)
    for (j in 0:(k - 1)) {
      l <- sort(tau_hat_all)[j + 1]
      r <- sort(tau_hat_all)[j + 1 + 1]
      if (r - l >= 2 * d) {
        t_lr <- rep(NA, r - l)
        count <- 0
        for (t in l:r) {
          if (t > l & t < r & min(t - l, r - t) >= d) {
            count <- count + 1
            t_lr[count] <- t
            if (is.na(all_seg[l + 1, t])) {
              all_seg[l + 1, t] <- get_cost(dat_smry, l + 1, t)
            }
            if (is.na(all_seg[t + 1, r])) {
              all_seg[t + 1, r] <- get_cost(dat_smry, t + 1, r)
            }
          }
        }
        temp <- all_seg[l + 1, r] - (all_seg[l + 1, t_lr] + all_seg[t_lr + 1, r])
        temp_which_max <- which.max(temp)
        cost_cmp[j + 1] <- temp[temp_which_max]
        cps_cmp[j + 1] <- t_lr[temp_which_max]
      }
    }
    if (!all(is.na(cost_cmp))) {
      tau_hat_all[1 + k] <- cps_cmp[which.max(cost_cmp)]
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

WBS_custom_R <- function(dat_smry, n, L, d, lr_M, get_cost) {

  out <- list()

  M <- nrow(lr_M)
  all_seg <- matrix(NA, n, n)
  for (i in 1:M) {
    all_seg[lr_M[i, 1] + 1, lr_M[i, 2]] <- get_cost(dat_smry, lr_M[i, 1] + 1, lr_M[i, 2])
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
          if (ri - li >= 2 * d) {
            t_lr <- rep(NA, ri - li)
            count <- 0
            for (t in li:ri) {
              if (t > li & t < ri & min(t - li, ri - t) >= d) {
                count <- count + 1
                t_lr[count] <- t
                if (is.na(all_seg[li + 1, t])) {
                  all_seg[li + 1, t] <- get_cost(dat_smry, li + 1, t)
                }
                if (is.na(all_seg[t + 1, ri])) {
                  all_seg[t + 1, ri] <- get_cost(dat_smry, t + 1, ri)
                }
              }
            }
            if (is.na(all_seg[li + 1, ri])) {
              all_seg[li + 1, ri] <- get_cost(dat_smry, li + 1, ri)
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

PELT_custom_R <- function(dat_smry, n, pen_val, d, K = 0, get_cost) {

  out <- list()

  all_seg <- matrix(NA, n, n)
  F_cost <- matrix(NA, n + 1, length(pen_val))
  F_cost[1, ] <- -pen_val
  for (ts in d:(2 * d - 1)) {
    all_seg[1, ts] <- get_cost(dat_smry, 1, ts)
    F_cost[ts + 1, ] <- all_seg[1, ts]
  }
  cp <- matrix(0, n, length(pen_val))
  cps_final <- matrix(0, n, length(pen_val))
  for (b in 1:length(pen_val)) {
    R <- matrix(NA, n, n + 1)
    R[1, 2 * d] <- 0
    R[2, 2 * d] <- d
    for (ts in (2 * d):n) {
      R_finite <- R[!is.na(R[, ts]), ts]
      temp <- rep(NA, length(R_finite))
      for (i in 1:length(R_finite)) {
        if (is.na(all_seg[R_finite[i] + 1, ts])) {
          all_seg[R_finite[i] + 1, ts] <- get_cost(dat_smry, R_finite[i] + 1, ts)
        }
        temp[i] <- F_cost[R_finite[i] + 1, b] + all_seg[R_finite[i] + 1, ts] + pen_val[b]
      }
      idx <- which.min(temp)
      F_cost[ts + 1, b] <- temp[idx]
      cp[ts, b] <- R_finite[idx]
      R_prune <- R_finite[which(temp + K < F_cost[ts + 1, b] + pen_val[b])]
      R[1:length(R_prune), ts + 1] <- R_prune
      R[length(R_prune) + 1, ts + 1] <- ts - (d - 1)
    }
    cp0 <- cp[n, b]
    count <- 1
    while (cp0 > 0) {
      cps_final[count, b] <- cp0
      cp0 <- cp[cp0, b]
      count <- count + 1
    }
  }

  out$cpt_cand <- cps_final
  return(out)
}
