################################################################################
#                                                                              #
#              DCSmooth Package: Additional Functions for SARMA                #
#                                                                              #
################################################################################

### Includes the addtional functions and sub-functions for SARMA estimation

  # sarma.statTest
    # sarma.charactF
  # qarma.est_3rdstep (UNUSED)
  # .qarma.fill_lag_matrix
  # qarma.yw_matrix
    # .matrix_acf
    # .matrix_acf_positive
    # .matrix_acf_negative
  # qarma.ssde
    # qarma.spectral.density
  # sarma.order.gpac
  # sarma.order.aic.bic

#-------------------------Test for QARMA stationarity--------------------------#

# Test Function
sarma.statTest = function(ar)
{
  # make set.seed locally
  old_state = get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit(assign(".Random.seed", old_state, envir = .GlobalEnv, 
                 inherits = FALSE))

  l_bound = c(0, 0, 0, 0)
  u_bound = c(2*pi, 2*pi, 1, 1)

  # set.seed(123)
  for (k in 1:50)
  {
    phi = stats::runif(2) * 2*pi
    rad = sqrt(stats::runif(2)) * 1.0
    
    theta_init = c(phi, rad)
    opt = stats::optim(theta_init, sarma.charactF, ar = ar, method = "L-BFGS-B",
                lower = l_bound, upper = u_bound)
    
    z1c = complex(modulus = opt$par[3], argument = opt$par[1])
    z2c = complex(modulus = opt$par[4], argument = opt$par[2])
    
    if ((abs(z1c) < 1) && (abs(z2c) < 1) && opt$value < 0.000001)
    {
      return(FALSE)
    }
  }
    
    return(TRUE)
}

# Compute characteristic function of AR part
sarma.charactF = function(theta, ar)
{
  phi1 = theta[1]
  phi2 = theta[2]
  rad1 = theta[3]
  rad2 = theta[4]
  z1c = complex(modulus = rad1, argument = phi1)
  z2c = complex(modulus = rad2, argument = phi2)
  
  ar_x = dim(ar)[1] - 1; ar_t = dim(ar)[2] - 1
  
  # set up vectors for z^i
  zx_vec = z1c^(0:ar_x)
  zt_vec = z2c^(0:ar_t)
  
  out = t(zx_vec) %*% ar %*% zt_vec
  
  return(abs(out))
}

sarma.char = function(z1c, z2c, ar)
{
  phi1 = Arg(z1c)
  phi2 = Arg(z2c)
  rad1 = Mod(z1c)
  rad2 = Mod(z2c)
  
  theta = c(phi1, phi2, rad1, rad2)
  
  return(sarma.charactF(theta, ar))
}

#---------------------3rd Step for HR-Algorithm (UNUSED)-----------------------#

# # Function for refinement of QARMA estimation
# # (does not seem to provide a significant improvement)
# 
# 
# # TODO tidy up function, include .qarma.fill_lag matrix instead of loops
# qarma.est_3rdstep = function(Y, ar_mat, ma_mat, model_order)
# {
#   nX = dim(Y)[1]; nT = dim(Y)[2]
#   
#   # value (x,t) of ma_mat has to be zero
#   ma_mat_0 = ma_mat; ma_mat_0[1, 1] = 0
#   ar_mat_0 = ar_mat; ar_mat_0[1, 1] = 0
#   
#   # calculate ARMA-orders for different purposes
#   max_lag_x = max(model_order$ar[1], model_order$ma[1])
#   max_lag_t = max(model_order$ar[2], model_order$ma[2])
#   total_lag_ar = (model_order$ar[1] + 1) * (model_order$ar[2] + 1) - 1
#   total_lag_ma = (model_order$ma[1] + 1) * (model_order$ma[2] + 1) - 1
#   
#   z_matrix = matrix(0, nrow = nX, ncol = nT)
#   v_matrix = w_matrix = z_matrix
#   
#   for (i in (max_lag_x + 1):nX)
#   {
#     for (j in (max_lag_t + 1):nT)
#     {
#       ind_ar_x = i:(i - model_order$ar[1])
#       ind_ar_t = j:(j - model_order$ar[2])
#       ind_ma_x = i:(i - model_order$ma[1])
#       ind_ma_t = j:(j - model_order$ma[2])
#       
#       # Strange Part here
#       z_matrix[i, j] = sum(ar_mat * Y[ind_ar_x, ind_ar_t]) -
#         sum(ma_mat_0 * z_matrix[ind_ma_x, ind_ma_t])
#       v_matrix[i, j] = sum(-ar_mat_0 * v_matrix[ind_ar_x, ind_ar_t]) +
#         z_matrix[i, j]
#       w_matrix[i, j] = sum(-ma_mat_0 * w_matrix[ind_ma_x, ind_ma_t]) +
#         z_matrix[i, j]
#     }
#   }
#   
#   zvw_lm_matrix = data.frame(matrix(NA,
#                                     nrow = (nX - max_lag_x) * (nT - max_lag_t),
#                                     ncol = total_lag_ar + total_lag_ma + 1))
#   zvw_lm_matrix[, 1] =
#     as.vector(z_matrix[(max_lag_x + 1):nX, (max_lag_t + 1):nT])
#   names(zvw_lm_matrix)[1] = "Z"
#   
#   for (i in 0:model_order$ar[1])
#   {
#     for (j in 0:model_order$ar[2])
#     {
#       if (!(i == 0 && j == 0))
#       {
#         zvw_lm_matrix[, i*(model_order$ar[2] + 1) + j + 1] =
#           as.vector(v_matrix[(max_lag_x + 1):nX - i, (max_lag_t + 1):nT - j])
#         names(zvw_lm_matrix)[i*(model_order$ar[2] + 1) + j + 1] =
#           paste0("V_", i, j)
#       }
#     }
#   }
#   for (i in 0:model_order$ma[1])
#   {
#     for (j in 0:model_order$ma[2])
#     {
#       if (!(i == 0 && j == 0))
#       {
#         zvw_lm_matrix[, total_lag_ar + i*(model_order$ma[2] + 1) + j + 1] =
#           as.vector(w_matrix[(max_lag_x + 1):nX - i, (max_lag_t + 1):nT - j])
#         names(zvw_lm_matrix)[total_lag_ar + i*(model_order$ma[2] + 1) + j + 1] =
#           paste0("W_", i, j)
#       }
#     }
#   }
#   
#   # linear regression
#   zvw_lm = stats::lm(Z ~ . + 0, data = zvw_lm_matrix)
#   
#   return(zvw_lm$coef)
# }

#-----------------------------Auxiliary Functions------------------------------#

.qarma.fill_lag_matrix = function(Y, lag_x, lag_t, include_00 = TRUE,
                                  name_prefix)
{
  nX = dim(Y)[1]; nT = dim(Y)[2]
  lag_obs = (nX - lag_x) * (nT - lag_t)
  lag_n = (lag_x + 1) * (lag_t + 1)
  
  matrix_out = data.frame(matrix(NA, nrow = lag_obs, ncol = lag_n))
  
  for (i in 0:lag_x) # use lag orders as loop indices
  {
    for (j in 0:lag_t)
    {
      index_x = (lag_x - i + 1):(nX - i)
      index_t = (lag_t - j + 1):(nT - j)
      matrix_out[, (j + 1) + i * (lag_t + 1)] = as.vector(Y[index_x, index_t])
      colnames(matrix_out)[(j + 1) + i * (lag_t + 1)] = 
        paste0(name_prefix, "_", i, j)
    }
  }
  
  if (include_00 == FALSE && lag_n > 1)
  {
    matrix_out = matrix_out[2:lag_n]
  } else if (include_00 == FALSE && lag_n == 1) {
    matrix_out = matrix_out * 0
  }
  
  return(matrix_out)
}

#-------------------Yule-Walker-Estimation Function for AR---------------------#

qarma.yw_matrix = function(Y, ar_order = c(1, 1), ma_order = c(0, 0))
{
  # set up parameters, result matrices and vectors etc.
  nX = dim(Y)[1]; nT = dim(Y)[2]; n = nX * nT
  p1_ar = ar_order[1]
  p2_ar = ar_order[2]
  d_ar = (p1_ar + 1) * (p2_ar + 1) - 1
  
  acf_matrix_ar = matrix(NA, nrow = d_ar, ncol = d_ar)
  acf_vector_ar = vector(mode = "numeric", length = d_ar)
  # st_mat is over rows first (i, j) -> (i - 1, j), which is normal R order
  st_mat = expand.grid(p1 = c(p1_ar:0), p2 = c(p2_ar:0))
  st_mat = st_mat[order(st_mat[, 1], st_mat[, 2]), ]
  
  # calculate acf_matrix and acf_vector
  for (k in 1:d_ar)
  {
    for (l in 1:d_ar)
    {
      st_vec = st_mat[k + 1, ] - st_mat[l + 1, ] + ma_order
      acf_matrix_ar[k, l] = .matrix_acf(Y, st_vec)
    }
    acf_vector_ar[k] = .matrix_acf(Y, st_mat[k + 1, ] + ma_order)
  }
  
  # solving Yule-Walker equations
  yw.estimators = solve(acf_matrix_ar) %*% acf_vector_ar
  phi_ar_matrix = matrix(c(1, -yw.estimators), nrow = (p1_ar + 1),
                         ncol = (p2_ar + 1), byrow = TRUE)
  
  return(phi_ar_matrix)
}

# auxiliary functions for Yule-Walker estimation
.matrix_acf = function(Y, st_vec)
{
  s = as.numeric(st_vec[1])
  t = as.numeric(st_vec[2])
  n = dim(Y)
  
  if ((s >= 0 && t >= 0) || (s < 0 && t < 0))
  {
    acf_out = .matrix_acf_positive(Y, abs(s), abs(t), n)
  } else if (s < 0 && t >= 0) {
    acf_out = .matrix_acf_negative(Y, s = -s, t, n)
  } else {
    acf_out = .matrix_acf_negative(Y, s, t = -t, n)
  }
  
  # unbiased estimator from Ha/Newton(1993)
  unbiased_factor = (n[1] - s)*(n[2] - t)/prod(n)
  return(acf_out/unbiased_factor)
}

.matrix_acf_positive = function(Y, s, t, n)
{
  Y0 = Y[1:(n[1] - s), 1:(n[2] - t)]
  Y1 = Y[(s + 1):n[1], (t + 1):n[2]]
  acf_out = sum(Y0 * Y1)
  return(acf_out)
}

.matrix_acf_negative = function(Y, s, t, n)
{
  Y0 = Y[1:(n[1] - s), (t + 1):n[2]]
  Y1 = Y[(s + 1):n[1], 1:(n[2] - t)]
  acf_out = sum(Y0 * Y1)
  return(acf_out)
}

#----------------------Calculation of spectral density-------------------------#

qarma.ssde = function(Y, ar, ma, st_dev)
{
  X = T = seq(from = -pi, to = pi, length.out = 100)
  y_spectrum = matrix(NA, nrow = 100, ncol = 100)
  
  for (i in 1:100)
  {
    for (j in 1:100)
    {
      omega = c(X[i], T[j])
      y_spectrum[i, j] = qarma.spectral.density(Y, ar, ma, st_dev, omega)
    }
  }
  return(y_spectrum)
}

qarma.spectral.density = function(Y, ar, ma, st_dev, omega)
{
  lag_ar = dim(ar) - 1; lag_ma = dim(ma) - 1
  z1 = complex(argument = omega[1]); z2 = complex(argument = omega[2])
  
  g00 = stats::sd(Y)^2
  
  ar_sum = Re(sum(ar * (z1^(lag_ar[1]:0)) %*% t(z2^(lag_ar[2]:0))) * 
                sum(ar * ((1/z1)^(lag_ar[1]:0)) %*% t((1/z2)^(lag_ar[2]:0))))
  ma_sum = Re(sum(ma * (z1^(lag_ma[1]:0)) %*% t(z2^(lag_ma[2]:0))) * 
                sum(ma * ((1/z1)^(lag_ma[1]:0)) %*% t((1/z2)^(lag_ma[2]:0))))
  
  spectral_dens_out = st_dev^2/g00 * ma_sum/ar_sum
  
  return(spectral_dens_out)
}

#---------------------------Model Order Selection------------------------------#

# Order selection by GPAC (see Illig/Truong-Van (2006))
sarma.order.gpac = function(Y, order_max = list(ar = c(1, 1), ma = c(1, 1)))
{
  # set up vectors for ar and ma
  ar_vec_test = expand.grid(0:order_max$ar[1], 0:order_max$ar[2])
  ar_vec_test = ar_vec_test[2:dim(ar_vec_test)[1], ]
  names(ar_vec_test) = NULL
  ar_vec = expand.grid(0:(order_max$ar[1] + 1), 0:(order_max$ar[2] + 1))
  ar_vec = ar_vec[2:dim(ar_vec)[1], ]
  ar_vec = ar_vec[order(ar_vec[, 1], ar_vec[, 2]), ]
  names(ar_vec) = NULL
  #ar_vec = rbind(ar_vec, ar_vec[dim(ar_vec)[1], ] + 1)
  ma_vec = expand.grid(0:(order_max$ma[1]), 0:(order_max$ma[2]))
  ma_vec = ma_vec[order(ma_vec[, 1], ma_vec[, 2]), ]
  names(ma_vec) = NULL
  #ma_vec = rbind(ma_vec, ma_vec[dim(ma_vec)[1], ] + 1)
  
  # set up matrix for results of yw estimation
  gpac_mat = matrix(NA, nrow = dim(ma_vec)[1], ncol = dim(ar_vec)[1])
  colnames(gpac_mat) = paste0("(", ar_vec[, 1], ", ", ar_vec[, 2], ")")
  rownames(gpac_mat) = paste0("(", ma_vec[, 1], ", ", ma_vec[, 2], ")")
  ar_test_names = paste0("(", ar_vec_test[, 1], ", ", ar_vec_test[, 2], ")")
  
  # calculate GPAC
  for (i in 1:dim(ar_vec)[1])
  {
    for (j in 1:dim(ma_vec)[1])
    {
      ar = ar_vec[i, ]
      ma = ma_vec[j, ]
      gpac_mat[j, i] = qarma.yw_matrix(Y, ar_order = as.numeric(ar), 
                                       ma_order = as.numeric(ma))[unlist(ar)[1] + 1,
                                                                  unlist(ar)[2] + 1]
    }
  }
  
  # find zeros of GPAC
  nRow = dim(gpac_mat)[1]; nCol = dim(gpac_mat)[2]
  gpac_count = gpac_mat*0 + 1
  
  # values chosen by hand, might be improved  
  gpac_count[which(abs(gpac_mat) < max(stats::quantile(abs(gpac_mat), 0.35), 0.1)
                   , arr.ind = TRUE)] = 0
  
  count_zeros = 1:nRow*0
  
  ar_out = vector(mode = "numeric")
  ma_out = vector(mode = "numeric")
  # score = vector(mode = "numeric")
  
  for (i in 1:nRow)
  {
    for (j in 1:(prod(order_max$ar + 1) - 1))
    {
      ar = as.vector(ar_vec_test[j, ], mode = "numeric")
      jCol = which(ar_test_names[j] == colnames(gpac_count))
      colIndex = which(as.logical(vapply(ar[1], '<=', 
                                         logical(length(ar_vec[, 1])), ar_vec[, 1]) *
                                    vapply(ar[2], '<=', 
                                           logical(length(ar_vec[, 2])), ar_vec[, 2])))
      gpac_count_sub = gpac_count[i, colIndex]
      if ((gpac_count_sub[1] == 1) & (sum(gpac_count_sub) == 1))
      {
        ar_out = rbind(ar_out, ar)
        ma_out = rbind(ma_out, ma = as.vector(ma_vec[i, ], mode = "numeric"))
        # score = rbind(score, sum(gpac_mat[i, colIndex]^2))
      }
      
    }
  }
  
  if (length(ar_out) != 0)
  {
    # ar_out = ar_out[which.min(score), ]
    # ma_out = ma_out[which.min(score), ]
    ar_out = ar_out[dim(ar_out)[1], ]
    ma_out = ma_out[dim(ma_out)[1], ]
  } else if (length(ar_out) == 0) {
    warning("No order selection by GPAC possible, iid. case is assumed")
    ar_out = c(0, 0)
    ma_out = c(0, 0)
  }
  
  if (length(ar_out) == 0) {
    warning("No order selection by GPAC possible, iid. case is assumed")
    ar_out = c(0, 0)
    ma_out = c(0, 0)
  }
  
  return(list(ar = ar_out, ma = ma_out))
}


sarma.order.aic.bic <- function(Rmat, pmax = c(1, 1), qmax = c(1, 1),
                                crit = "bic", restr = NULL, sFUN = min,
                                parallel = FALSE)
{
  if(crit == "bic") {
    crit.fun = stats::BIC
  }
  else if (crit == "aic") {
    crit.fun = stats::AIC
  }
  
  bic_x =  matrix(0, pmax[1] + 1, qmax[1] + 1)
  bic_t =  matrix(0, pmax[2] + 1, qmax[2] + 1)
  R_x = as.vector(Rmat)
  R_t = as.vector(t(Rmat))
  
  if (parallel == TRUE)
  {
    n.cores = parallel::detectCores(logical = TRUE) - 1
    doParallel::registerDoParallel(n.cores)
    
    `%dopar%` = foreach::`%dopar%`
    `%:%` = foreach::`%:%`
    
    bic_x = foreach::foreach(i = 1:(pmax[1] + 1), .combine = "rbind") %:%
      foreach::foreach(j = 1:(qmax[1] + 1), .combine = "c") %dopar% {
        bic = crit.fun(suppressWarnings(stats::arima(R_x,
                       order = c(i - 1, 0, j - 1), include.mean = FALSE)))
      }
    
    bic_t = foreach::foreach(i = 1:(pmax[2] + 1), .combine = "rbind") %:%
      foreach::foreach(j = 1:(qmax[2] + 1), .combine = "c") %dopar% {
        bic = crit.fun(suppressWarnings(stats::arima(R_t,
                       order = c(i - 1, 0, j - 1), include.mean = FALSE)))
      }
    
    doParallel::stopImplicitCluster()
  } else {
    for (i in 1:(pmax[1] + 1))
    {
      for (j in 1:(qmax[1] + 1))
      {
        bic_x[i, j] = crit.fun(suppressWarnings(stats::arima(R_x,
                          order = c(i - 1, 0, j - 1), include.mean = FALSE)))
      }
    }
    for (i in 1:(pmax[2] + 1))
    {
      for (j in 1:(qmax[2] + 1))
      {
        bic_t[i, j] = crit.fun(suppressWarnings(stats::arima(R_t,
                          order = c(i - 1, 0, j - 1), include.mean = FALSE)))
      }
    }
  }
  
  restr = substitute(restr)
  if(!is.null(restr)){
    ord.opt_x <- c(which(bic_x == sFUN(bic_x[eval(restr)]), arr.ind = TRUE) - 1)
    ord.opt_t <- c(which(bic_t == sFUN(bic_t[eval(restr)]), arr.ind = TRUE) - 1)
  } else {
    ord.opt_x <- c(which(bic_x == sFUN(bic_x), arr.ind = TRUE) - 1)
    ord.opt_t <- c(which(bic_t == sFUN(bic_t), arr.ind = TRUE) - 1)
  }
  
  # put model_orders into list
  ar = c(ord.opt_x[1], ord.opt_t[1])
  ma = c(ord.opt_x[2], ord.opt_t[2])
  model_order = list(ar = ar, ma = ma)
  
  return(model_order)   
}