################################################################################
#                                                                              #
#       DCSmooth Package: Auxiliary Functions for Long Memory Bandwidths       #
#                                                                              #
################################################################################

### Contains functions needed in the bandwidth selection under long memory
### spatial errors.

  # h.opt.LM
  # sfarima.cf
  # h.coef.cb
  # kernel.prop.LM

#----------------------Formula for optimal bandwidths--------------------------#

h.opt.LM = function(mxx, mtt, var_coef, var_model, n_sub, dcs_options, n_x, n_t)
{
  if (dcs_options$type == "LP")
  {
    p_order = dcs_options$p_order
  } else if (dcs_options$type == "KR") {
    p_order = c(1, 1)
  }
  
  drv_vec = dcs_options$drv
  shrink_par = dcs_options$IPI_options$trim
  k_vec = as.numeric(substr(dcs_options$kern, nchar(dcs_options$kern) - 2,
                             nchar(dcs_options$kern) - 2))
  mu_vec = as.numeric(substr(dcs_options$kern, nchar(dcs_options$kern) - 1,
                             nchar(dcs_options$kern) - 1))
  
  d_vec = var_model$d
  # calculation of integrals
  i11 = sum(mxx^2)/n_sub; i22 = sum(mtt^2)/n_sub; i12 = sum(mxx * mtt)/n_sub
  
  # kernel constants (kernel Functions may also depend on p, drv)
  kernel_prop_x = kernel.prop.LM(k_vec[1], p_order[1], drv_vec[1], d_vec[1],
                                 mu_vec[1])
  kernel_prop_t = kernel.prop.LM(k_vec[2], p_order[2], drv_vec[2], d_vec[2],
                                 mu_vec[2])
  
  # compute additional values
  cb = h.coef.cb(i11, i22, i12, kernel_prop_x, kernel_prop_t, drv_vec, p_order,
                 d_vec)
  cn = n_t/n_x
  C1 = 4 * var_coef * kernel_prop_x$V * kernel_prop_t$V
  delta = p_order[1] + 1 - drv_vec[1]
  
  C1A = (1 - 2*d_vec[1] + 2*drv_vec[1]) * C1 * cn^(diff(d_vec)) /
    (2 * delta * kernel_prop_x$mu * cb^(1 - 2*d_vec[2] + 2*drv_vec[2]) *
       (kernel_prop_x$mu * i11 + kernel_prop_t$mu * i12 * cb^delta)) *
    (1 - 2*shrink_par[1]) * (1 - 2*shrink_par[2])
  C2A = (1 - 2*d_vec[2] + 2*drv_vec[2]) * C1 * cn^(diff(d_vec)) /
    (2 * delta * kernel_prop_t$mu * cb^(1 - 2*d_vec[2] + 2*drv_vec[2]) *
       (kernel_prop_x$mu * i12 * cb^(-delta) + kernel_prop_t$mu * i22)) *
    (1 - 2*shrink_par[1]) * (1 - 2*shrink_par[2])
  
  b1A = (C1A / (n_x * n_t)^(1 - sum(d_vec)))^(1/
                   (2*(1 - sum(d_vec) + delta + sum(drv_vec))))
  b2A = (C2A / (n_x * n_t)^(1 - sum(d_vec)))^(1/
                   (2*(1 - sum(d_vec) + delta + sum(drv_vec))))
  
  return(c(b1A, b2A))
}

sfarima.cf = function(R_mat, model_order =
                              list(ar = c(1, 1), ma = c(1, 1)))
{
  sfarima = sfarima.est(R_mat, model_order = model_order)
  
  cf_est = 1/(2*pi)^2 * sum(sfarima$model$ma)^2 / sum(sfarima$model$ar)^2 *
            sfarima$model$sigma^2
  
  return(list(cf_est = cf_est, var_model = sfarima))
}

#------------------------Formula for coefficient cb----------------------------#

h.coef.cb = function(i11, i22, i12, kernel_prop_1, kernel_prop_2, drv_vec,
                     p_order, d_vec)
{
  denom_value = (1 - 2*d_vec[1] + 2*drv_vec[1]) * kernel_prop_2$mu^2 * i22
  sec_term = (diff(d_vec) - diff(drv_vec)) * kernel_prop_1$mu *
    kernel_prop_2$mu * i12
  sqrt_value = sec_term^2 + denom_value *
    (1 - 2*d_vec[2] + 2*drv_vec[2]) * kernel_prop_1$mu^2 * i11
  
  delta = p_order[1] + 1 - drv_vec[1]
  cb_return = 1/denom_value * (sqrt(sqrt_value) - sec_term)
  
  return(cb_return^(1/delta))
}

#------------------------------Long-Memory KDF---------------------------------#

kernel.prop.LM = function(k, p, drv, d, mu)
{
  n_int = 20000
  u_seq  = seq(from = -1, to = 1, length.out = 2*n_int + 1)
    
  if (p == 1)
  {
    val_mu = sum(u_seq^k * (1 - u_seq^2)^mu) / n_int
    val_V  = gamma(1 - 2 * d) * sin(pi * d) *
              lookup$p1p3_lookup[mu + 1, 1][[1]](d)
  } else if (p == 3) {
    val_mu = sum(u_seq^k * lookup$p3_lookup[mu + 1][[1]](u_seq)) / n_int
    val_V  = gamma(1 - 2 * d) * sin(pi * d) *
              lookup$p1p3_lookup[mu + 1, 2][[1]](d)
  }
  
  return(list(V = val_V, mu = val_mu))
}