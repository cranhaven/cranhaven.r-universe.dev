################################################################################
#                                                                              #
#       DCSmooth Package: Auxiliary Functions for Bandwidth Selection          #
#                                                                              #
################################################################################

### Contains functions needed in the bandwidth selection (if not long memory
### estimation is used).

  # h.opt.LP
  # h.opt.KR
  # integral.calc.KR
  # kernel.prop.LP
  # kernel.prop.KR
  # inflation.LP
  # inflation.KR

#----------------------Formula for optimal bandwidths--------------------------#

# Local Polynomial Regression
h.opt.LP = function(mxx, mtt, var_coef, n, n_sub, p_order, drv_vec, kernel_x,
                    kernel_t)
{
  # calculation of integrals
  i11 = sum(mxx^2)/n_sub; i22 = sum(mtt^2)/n_sub; i12 = sum(mxx * mtt)/n_sub
  
  # kernel constants (kernel Functions may also depend on p, drv)
  kernel_prop_1 = kernel.prop.LP(kernel_x, p_order[1])
  kernel_prop_2 = kernel.prop.LP(kernel_t, p_order[2])
  
  # relation factor gamma (h_1 = gamma * h_2)
  delta = (p_order - drv_vec)[1] # should be the same for both entries
  gamma_delta = kernel_prop_2$mu/kernel_prop_1$mu *
                   ( -i12/i11 * diff(drv_vec)/(2*drv_vec[2] + 1) +
                   c(sqrt(i12^2/i11^2 * diff(drv_vec)^2 / (2*drv_vec[2] + 1)^2 +
                     i22/i11 * (2*drv_vec[1] + 1)/(2*drv_vec[2] + 1)),
                    -sqrt(i12^2/i11^2 * diff(drv_vec)^2 / (2*drv_vec[2] + 1)^2 +
                     i22/i11 * (2*drv_vec[1] + 1)/(2*drv_vec[2] + 1))) )
  gamma_delta = gamma_delta[which(gamma_delta > 0)]
  gamma = gamma_delta^(1/(delta + 1))
  
  # optimal bandwidths
  C1 = kernel_prop_1$mu^2 * i11 + kernel_prop_1$mu * kernel_prop_2$mu * 
            i12 / gamma_delta
  hx_opt = (2*drv_vec[1] + 1)/(2*(delta + 1)) * (kernel_prop_1$R *
           kernel_prop_2$R * var_coef) /
           (n / gamma^(2*drv_vec[1] + 1) * C1) 
  C2 = kernel_prop_2$mu^2 * i22 + kernel_prop_1$mu * kernel_prop_2$mu * 
           i12 * gamma_delta
  ht_opt = (2*drv_vec[2] + 1)/(2*(delta + 1)) * (kernel_prop_1$R *
           kernel_prop_2$R * var_coef) /
           (n * gamma^(2*drv_vec[2] + 1) * C2)
  
  hx_opt = hx_opt^(1/(2*(delta + sum(drv_vec) + 2)))
  ht_opt = ht_opt^(1/(2*(delta + sum(drv_vec) + 2)))
  
  return(c(hx_opt, ht_opt))
}

# Kernel Regression
h.opt.KR = function(mxx, mtt, var_coef, n, n_sub, k_vec, drv, kernel_prop_x, 
                    kernel_prop_t)
{
  # calculation of integrals
  i11 = sum(mxx^2)/n_sub; i22 = sum(mtt^2)/n_sub; i12 = sum(mxx * mtt)/n_sub
  
  # delta = k - \nu = 2 fixed.
  delta = 2

  alpha_delta = kernel_prop_t$mu/kernel_prop_x$mu *
                ( -i12/i11 * diff(drv)/(2*drv[2] + 1) +
                c(sqrt(i12^2/i11^2 * diff(drv)^2/(2*drv[2] + 1)^2 +
                  i22/i11 * (2*drv[1] + 1)/(2*drv[2] + 1)),
                  -sqrt(i12^2/i11^2 * diff(drv)^2/(2*drv[2] + 1)^2 +
                  i22/i11 * (2*drv[1] + 1)/(2*drv[2] + 1)) ) )
  alpha_delta = alpha_delta[which(alpha_delta > 0)]
  alpha = alpha_delta^(1/delta)

  C1 = kernel_prop_x$mu^2 * i11 +
       kernel_prop_x$mu * kernel_prop_t$mu * i12 / alpha_delta
  C2 = kernel_prop_t$mu^2 * i22 +
       kernel_prop_x$mu * kernel_prop_t$mu * i12 * alpha_delta

  hx = { (2*drv[1] + 1)/(2*delta) *
         (kernel_prop_x$R * kernel_prop_t$R * var_coef) /
         (n * alpha^(-(2*drv[2] + 1)) * C1)
       }^{1/(2 + 2*delta + 2*sum(drv))}
  ht = { (2*drv[2] + 1)/(2*delta) *
         (kernel_prop_x$R * kernel_prop_t$R * var_coef) /
         (n * alpha^(2*drv[1] + 1) * C2)
       }^{1/(2 + 2*delta + 2*sum(drv))}
  
  return(c(hx, ht))
}

#-------------------------Kernel property calculation--------------------------#

kernel.prop.LP = function(kernel_fcn, p, n_int = 5000)
{
  u_seq  = seq(from = 1, to = -1, length.out = (2 * n_int + 1))
  
  val_R  = sum(kernel_fcn(u_seq, q = 1)^2)/n_int
  val_mu = sum(kernel_fcn(u_seq, q = 1) * u_seq^(p + 1)) /
    (n_int * factorial(p + 1))
  
  return(list(R = val_R, mu = val_mu))
}

kernel.prop.KR = function(kernel_fcn, k, n_int = 5000)
{
  u_seq  = seq(from = 1, to = -1, length.out = (2 * n_int + 1))
  
  val_R  = sum(kernel_fcn_use(u_seq, q = 1, kernel_fcn)^2)/n_int
  val_mu = sum(kernel_fcn_use(u_seq, q = 1, kernel_fcn) * u_seq^k) /
    (n_int * factorial(k))
  
  return(list(R = val_R, mu = val_mu))
}

#-----------------bandwidth inflation for derivative estimations---------------#

inflation.LP = function(h, dcs_options, n_x, n_t)
{
  if (dcs_options$IPI_options$infl_exp[1] == "auto")
  {
    infl_exp = c(0, 0)
    infl_exp[1] = (dcs_options$p_order[1] + dcs_options$drv[2] + 2)/
                  (sum(dcs_options$p_order) + 3)
    infl_exp[2] = (dcs_options$p_order[2] + dcs_options$drv[1] + 2)/
                  (sum(dcs_options$p_order) + 3)
  } else {
    infl_exp = dcs_options$IPI_options$infl_exp
  }
  
  infl_par = dcs_options$IPI_options$infl_par
  h_infl_xx = c(infl_par[1] * h[1]^infl_exp[1], infl_par[2] * h[2]^infl_exp[1])
  h_infl_tt = c(infl_par[2] * h[1]^infl_exp[2], infl_par[1] * h[2]^infl_exp[2])
  
  # minimum values for LPR
  h_xx = pmax(h_infl_xx, (dcs_options$p_order + 2)/(c(n_x, n_t) - 1))
  h_tt = pmax(h_infl_tt, (dcs_options$p_order + 2)/(c(n_x, n_t) - 1))
  
  return(list(h_xx = h_xx, h_tt = h_tt))
}

inflation.KR = function(h, n, dcs_options)
{
  if (dcs_options$IPI_options$infl_exp[1] == "auto")
  {
    infl_exp = c(0, 0)
    infl_exp[1] = (dcs_options$p_order[1] + dcs_options$drv[2] + 2)/
      (sum(dcs_options$p_order) + 3)
    infl_exp[2] = (dcs_options$p_order[2] + dcs_options$drv[1] + 2)/
      (sum(dcs_options$p_order) + 3)
  } else {
    infl_exp = dcs_options$IPI_options$infl_exp
  }
  
  infl_par = dcs_options$IPI_options$infl_par
  h_infl_xx = c(infl_par[1] * h[1]^infl_exp[1], infl_par[2] * h[2]^infl_exp[1])
  h_infl_tt = c(infl_par[2] * h[1]^infl_exp[2], infl_par[1] * h[2]^infl_exp[2])
  
  # ensure no bandwidth > 0.5 (or 0.45) as KR cannot handle estimation windows
  # > 1
  h_infl_xx = pmin(h_infl_xx, c(0.45, 0.45))
  h_infl_tt = pmin(h_infl_tt, c(0.45, 0.45))  
  
  return(list(h_xx = h_infl_xx, h_tt = h_infl_tt))
}
