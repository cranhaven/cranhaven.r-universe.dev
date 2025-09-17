################################################################################
#                                                                              #
#                     DCSmooth Package: Estimation of SARMA                    #
#                                                                              #
################################################################################

### This file includes all functions related to the estimation of 
### SARMA-processes

  # sarma.HR.est
  # sarma.sep.est
  # sarma.RSS.est

#-----------------SARMA Estimation using Hannan-Rissanen Algorithm-------------#

sarma.HR.est = function(Y, model_order = list(ar = c(1, 1), ma = c(1, 1)))
{
  nX = dim(Y)[1]; nT = dim(Y)[2]
  
  # calculate ARMA-orders for different purposes
  max_lag_x = max(model_order$ar[1], model_order$ma[1])
  max_lag_t = max(model_order$ar[2], model_order$ma[2])
  total_lag_ar = (model_order$ar[1] + 1) * (model_order$ar[2] + 1) - 1
  total_lag_ma = (model_order$ma[1] + 1) * (model_order$ma[2] + 1) - 1
  max_lag_ar = max(model_order$ar)
  max_lag_ma = max(model_order$ma)
  
  ### AR-only auxiliary estimation model by YW-estimation ###
  m1_ar = max(max_lag_x, 1) + ifelse(total_lag_ma > 0, 2, 0) 
  # increase order of aux AR model here
  m2_ar = max(max_lag_t, 1) + ifelse(total_lag_ma > 0, 2, 0)
  ar_aux = qarma.yw_matrix(Y, ar_order = c(m1_ar, m2_ar))
  
  # calculate residuals from auxiliary AR model
  residuals_mat = matrix(0, nrow = nX - m1_ar, ncol = nT - m2_ar)
  for (i in 1:(nX - m1_ar))
  {
    for (j in 1:(nT - m2_ar))
    {
      residuals_mat[i, j] = sum(ar_aux * Y[(i + m1_ar):i, (j + m2_ar):j])
    }
  }
  
  if (total_lag_ma > 0)
  {
    # set up submatrices of Y, res for use in fill_matrix procedur
    Y_mat = Y[(m1_ar + model_order$ma[1] - model_order$ar[1] + 1):nX,
              (m2_ar + model_order$ma[2] - model_order$ar[2] + 1):nT]
    
    # set up matrix for complete arma (dimension (nX - ar_x)*(nT - ar_t))
    matrix_arma = cbind(.qarma.fill_lag_matrix(Y_mat, lag_x = model_order$ar[1],
                                lag_t = model_order$ar[2], include_00 = TRUE,
                                name_prefix = "ar"),
                .qarma.fill_lag_matrix(residuals_mat, lag_x = model_order$ma[1],
                                lag_t = model_order$ma[2], include_00 = FALSE,
                                name_prefix = "ma"))
    
    # regression for QARMA model
    arma_reg = stats::lm(ar_00 ~ . + 0, data = matrix_arma)
    
    # fill ar and ma matrices
    # updated to lag-order (phi_00/psi_00 in upper left corner)
    # ar_mat is lhs of QARMA-equation (phi_00 = 1)
    if (total_lag_ar > 0)
    {
      ar_mat = matrix(c(1, -arma_reg$coef[1:total_lag_ar]), byrow = TRUE,
                      nrow = (model_order$ar[1] + 1), 
                      ncol = (model_order$ar[2] + 1))
    } else if (total_lag_ar == 0) {
      ar_mat = as.matrix(1)
    }
    if (total_lag_ma > 0)
    {
      ma_mat = matrix(c(1, arma_reg$coef[(total_lag_ar + 1):
                      (total_lag_ar + total_lag_ma)]), byrow = TRUE,
                      nrow = (model_order$ma[1] + 1),
                      ncol = (model_order$ma[2] + 1))
    } else if (total_lag_ar == 0) {
      ma_mat = as.matrix(1)
    }
  } else {
    arma_reg = list(ar_aux = as.vector(ar_aux)[-1])
    
    # fill ar matrix (and ma matrix)
    ar_mat = matrix(c(1, arma_reg$ar_aux[1:total_lag_ar]),
                    nrow = (model_order$ar[1] + 1),
                    ncol = (model_order$ar[2] + 1))
    ma_mat = matrix(1)
    
    arma_reg$residuals = residuals_mat
  }
  
  innov = arma_reg$residuals #residuals_mat
  
  # improve = FALSE
  # if (improve == TRUE)
  # {
  #   stdev = sqrt(sum(innov^2)/((nX - max_lag_x - model_order$ar[1]) * 
  #                                (nT - max_lag_t - model_order$ar[2])))
  #   
  #   arma_reg$coef = qarma.est_3rdstep(Y, ar_mat, ma_mat, model_order) +
  #     arma_reg$coef
  #   
  #   ar_mat = matrix(c(1, -arma_reg$coef[1:total_lag_ar]), byrow = TRUE,
  #                   nrow = (model_order$ar[1] + 1), 
  #                   ncol = (model_order$ar[2] + 1))
  #   ma_mat = matrix(c(1, arma_reg$coef[(total_lag_ar + 1):
  #                   (total_lag_ar + total_lag_ma)]), byrow = TRUE,
  #                   nrow = (model_order$ma[1] + 1),
  #                   ncol = (model_order$ma[2] + 1))
  # }
  
  # check stationarity
  statTest = sarma.statTest(ar_mat)

  # preparation of output
  rownames(ar_mat) = paste0("lag ", 0:model_order$ar[1])
  colnames(ar_mat) = paste0("lag ", 0:model_order$ar[2])
  rownames(ma_mat) = paste0("lag ", 0:model_order$ma[1])
  colnames(ma_mat) = paste0("lag ", 0:model_order$ma[2])
  
  stdev = stats::sd(innov)
  coef_out = list(Y = Y, innov = innov, model = list(ar = ar_mat, ma = ma_mat,
                  sigma = stdev), stnry = statTest)
  class(coef_out) = "sarma"
  attr(coef_out, "subclass") = "est"
  return(coef_out)
}

# sarma.sep.est = function(Y, model_order = list(ar = c(1, 1), ma = c(1, 1)))
# {
#   nX = dim(Y)[1]; nT = dim(Y)[2]
#   
#   # calculate ARMA-orders for different purposes
#   max_lag_x = max(model_order$ar[1], model_order$ma[1])
#   max_lag_t = max(model_order$ar[2], model_order$ma[2])
#   total_lag_ar = (model_order$ar[1] + 1) * (model_order$ar[2] + 1) - 1
#   total_lag_ma = (model_order$ma[1] + 1) * (model_order$ma[2] + 1) - 1
#   max_lag_ar = max(model_order$ar)
#   max_lag_ma = max(model_order$ma)
#   
#   ### AR-only auxiliary estimation model by YW-estimation ###
#   m1_ar = max(max_lag_x, 1) + ifelse(total_lag_ma > 0, 2, 0) 
#   # increase order of aux AR model here
#   m2_ar = max(max_lag_t, 1) + ifelse(total_lag_ma > 0, 2, 0)
#   ar_x = c(1, -ar.yw(as.vector(Y), aic = FALSE, order.max = m1_ar)$ar)
#   ar_t = c(1, -ar.yw(as.vector(t(Y)), aic = FALSE, order.max = m2_ar)$ar)
#   ar_aux = ar_x %*% t(ar_t)
#   
#   # calculate residuals from auxiliary AR model
#   residuals_mat = matrix(0, nrow = nX - m1_ar, ncol = nT - m2_ar)
#   for (i in 1:(nX - m1_ar))
#   {
#     for (j in 1:(nT - m2_ar))
#     {
#       residuals_mat[i, j] = sum(ar_aux * Y[(i + m1_ar):i, (j + m2_ar):j])
#     }
#   }
#   
#   if (total_lag_ma > 0)
#   {
#     # set up submatrices of Y, res for use in fill_matrix procedur
#     Y_mat = Y[(m1_ar + model_order$ma[1] - model_order$ar[1] + 1):nX,
#               (m2_ar + model_order$ma[2] - model_order$ar[2] + 1):nT]
#     
#     # set up matrix for complete arma (dimension (nX - ar_x)*(nT - ar_t))
#     matrix_arma = cbind(.qarma.fill_lag_matrix(Y_mat, lag_x = model_order$ar[1],
#                                                lag_t = model_order$ar[2], include_00 = TRUE,
#                                                name_prefix = "ar"),
#                         .qarma.fill_lag_matrix(residuals_mat, lag_x = model_order$ma[1],
#                                                lag_t = model_order$ma[2], include_00 = FALSE,
#                                                name_prefix = "ma"))
#     
#     matrix_arma = matrix_arma[, which(grepl("0", names(matrix_arma)))]
#     
#     # regression for QARMA model
#     arma_reg = stats::lm(ar_00 ~ . + 0, data = matrix_arma)
#     
#     # fill ar and ma matrices
#     # updated to lag-order (phi_00/psi_00 in upper left corner)
#     # ar_mat is lhs of QARMA-equation (phi_00 = 1)
#     ar_mat = c(1, -arma_reg$coef[(model_order$ar[1] + 1):
#                                    (sum(model_order$ar))]) %*%
#       t(c(1, -arma_reg$coef[1:model_order$ar[1]]))
#     ma_mat = c(1, arma_reg$coef[(sum(model_order$ar) + model_order$ma[1] + 1):
#                                   sum(unlist(model_order))]) %*%
#       t(c(1, arma_reg$coef[(sum(model_order$ar) + 1):
#                            (sum(model_order$ar) + model_order$ma[1])]))
# 
#     if (total_lag_ar == 0)
#     {
#       ar_mat[1, 1] = 1
#     }
#   } else {
#     arma_reg = list(ar_aux = as.vector(ar_aux)[-1])
#     
#     # fill ar matrix (and ma matrix)
#     ar_mat = matrix(c(1, arma_reg$ar_aux[1:total_lag_ar]),
#                     nrow = (model_order$ar[1] + 1),
#                     ncol = (model_order$ar[2] + 1))
#     ma_mat = matrix(1)
#     
#     arma_reg$residuals = residuals_mat
#   }
#   
#   innov = arma_reg$residuals #residuals_mat
#   
#   improve = FALSE
#   if (improve == TRUE)
#   {
#     stdev = sqrt(sum(innov^2)/((nX - max_lag_x - model_order$ar[1]) * 
#                                  (nT - max_lag_t - model_order$ar[2])))
#     
#     arma_reg$coef = qarma.est_3rdstep(Y, ar_mat, ma_mat, model_order) +
#       arma_reg$coef
#     
#     ar_mat = matrix(c(1, -arma_reg$coef[1:total_lag_ar]), byrow = TRUE,
#                     nrow = (model_order$ar[1] + 1), 
#                     ncol = (model_order$ar[2] + 1))
#     ma_mat = matrix(c(1, arma_reg$coef[(total_lag_ar + 1):
#                                          (total_lag_ar + total_lag_ma)]), byrow = TRUE,
#                     nrow = (model_order$ma[1] + 1),
#                     ncol = (model_order$ma[2] + 1))
#   }
#   
#   # check stationarity
#   statTest = sarma.statTest(ar_mat)
#   
#   # preparation of output
#   rownames(ar_mat) = paste0("lag ", 0:model_order$ar[1])
#   colnames(ar_mat) = paste0("lag ", 0:model_order$ar[2])
#   rownames(ma_mat) = paste0("lag ", 0:model_order$ma[1])
#   colnames(ma_mat) = paste0("lag ", 0:model_order$ma[2])
#   
#   stdev = stats::sd(innov)
#   coef_out = list(Y = Y, innov = innov, model = list(ar = ar_mat, ma = ma_mat,
#                                                      sigma = stdev), stnry = statTest)
#   class(coef_out) = "sarma"
#   attr(coef_out, "subclass") = "est"
#   return(coef_out)
# }

#--------------------SARMA Estimation using a separable Model------------------#

sarma.sep.est = function(Y, model_order = list(ar = c(1, 1), ma = c(1, 1)))
{
  n_x = dim(Y)[1]; n_t = dim(Y)[2]
  max_lag_x = max(model_order$ar[1], model_order$ma[1])
  max_lag_t = max(model_order$ar[2], model_order$ma[2])

  arma_t = stats::arima(as.vector(t(Y)),
                 order = c(model_order$ar[2], 0, model_order$ma[2]),
                 include.mean = FALSE)
 
  arma_x = stats::arima(as.vector(Y),
                 order = c(model_order$ar[1], 0, model_order$ma[1]),
                 include.mean = FALSE)

  # build result matrices
  ar_mat = as.matrix(c(1, -arma_x$coef[seq_len(model_order$ar[1])]) %*%
    t(c(1, -arma_t$coef[seq_len(model_order$ar[2])])))
  ma_mat = as.matrix(c(1, arma_x$coef[model_order$ar[1] +
                              seq_len(model_order$ma[1])]) %*%
    t(c(1, arma_t$coef[model_order$ar[2] + seq_len(model_order$ma[2])])))

  innov = sarma.residuals(Y, list(ar = ar_mat, ma = ma_mat, sigma = NA))
  stdev = stats::sd(innov)
  stat_test = sarma.statTest(ar_mat)
  
  colnames(ar_mat) = paste0("lag ", 0:model_order$ar[2])
  rownames(ar_mat) = paste0("lag ", 0:model_order$ar[1])
  colnames(ma_mat) = paste0("lag ", 0:model_order$ma[2])
  rownames(ma_mat) = paste0("lag ", 0:model_order$ma[1])

  # prepare output
  coef_out = list(Y = Y, innov = innov, model = list(ar = ar_mat, ma = ma_mat,
                  sigma = stdev), stnry = stat_test)
  class(coef_out) = "sarma"
  attr(coef_out, "subclass") = "est"
  return(coef_out)
}

#------------------------SARMA Estimation using RSS----------------------------#

sarma.RSS.est = function(Y, model_order = list(ar = c(1, 1), ma = c(1, 1)))
{
  theta_init = rep(0, times = sum(unlist(model_order)))
  n_x = dim(Y)[1]; n_t = dim(Y)[2]
  theta_opt  = stats::optim(theta_init, sarma_rss, R_mat = Y, 
                            model_order = model_order,
                            method = "BFGS")
  
  # put coefficients into matrices
  ar_x = c(1, -theta_opt$par[seq_len(model_order$ar[1])])
  ar_t = c(1, -theta_opt$par[model_order$ar[1] + seq_len(model_order$ar[2])])
  ma_x = c(1, theta_opt$par[sum(model_order$ar) + seq_len(model_order$ma[1])])
  ma_t = c(1, theta_opt$par[sum(model_order$ar) + model_order$ma[1] + 
                              seq_len(model_order$ma[2])])
  
  # prepare results for output
  ar_mat = as.matrix(ar_x %*% t(ar_t))
  ma_mat = as.matrix(ma_x %*% t(ma_t))
  stdev = sqrt(theta_opt$value/(n_x * n_t))
  model = list(ar = ar_mat, ma = ma_mat, sigma = stdev)
  innov = sarma.residuals(R_mat = Y, model = model)
  
  # check stationarity
  statTest = sarma.statTest(ar_mat)
  
  colnames(ar_mat) = paste0("lag ", 0:model_order$ar[2])
  rownames(ar_mat) = paste0("lag ", 0:model_order$ar[1])
  colnames(ma_mat) = paste0("lag ", 0:model_order$ma[2])
  rownames(ma_mat) = paste0("lag ", 0:model_order$ma[1])

  coef_out = list(Y = Y, innov = innov, model = list(ar = ar_mat, ma = ma_mat,
                  sigma = stdev), stnry = statTest)
  class(coef_out) = "sarma"
  attr(coef_out, "subclass") = "est"
  return(coef_out)
}

#-----------------------Calculation of cf coefficient--------------------------#

sarma.residuals = function(R_mat, model)
{
  n_x = dim(R_mat)[1]; n_t = dim(R_mat)[2]
  k_x = min(50, n_x); k_t = min(50, n_t)
  
  # result matrices
  E_itm = R_mat * 0   # intermediate results
  E_fnl = R_mat * 0   # final results
  
  ar_x = ifelse(length(model$ar[-1, 1]) > 0, -model$ar[-1, 1], 0)
  ma_x = ifelse(length(model$ma[-1, 1]) > 0, model$ma[-1, 1], 0)
  ar_t = ifelse(length(model$ar[1, -1]) > 0, -model$ar[1, -1], 0)
  ma_t = ifelse(length(model$ma[1, -1]) > 0, model$ma[1, -1], 0)
  
  ar_inf_x = ar_coef(ar = ar_x, ma = ma_x, d = 0, k = k_x)
  ar_inf_t = ar_coef(ar = ar_t, ma = ma_t, d = 0, k = k_t)
  
  for (j in 1:n_t)
  {
    E_itm[, j] = R_mat[, j:max(1, j - k_t + 1), drop = FALSE] %*%
      ar_inf_t[1:min(j, k_t), drop = FALSE]
  }
  
  for (i in 1:n_x)
  {
    E_fnl[i, ] = ar_inf_x[1:min(i, k_x), drop = FALSE] %*%
      E_itm[i:max(1, i - k_x + 1), , drop = FALSE]
  }

  return(E_fnl)
}