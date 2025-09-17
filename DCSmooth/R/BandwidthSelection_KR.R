###############################################################################
#                                                                             #
#         DCSmooth Package: R-Functions for KR Bandwidth Selection            #
#                                                                             #
###############################################################################

### Bandwidth selection function for kernel regression

  # KR.bndw

#------------------Function for the optimal bandwidth via IPI-----------------#

KR.bndw = function(Y, dcs_options, add_options, cf_est)
{
  n_x = dim(Y)[1]; n_t = dim(Y)[2]
  n  = n_x * n_t                            # total number of observations
  
  # set variables for weight type
  kern_type_vec = sub("^([[:alpha:]]*).*", "\\1", dcs_options$kerns)
                                                # extract weighting type
  k_vec = as.numeric(substr(dcs_options$kerns, nchar(dcs_options$kern) - 2,
                             nchar(dcs_options$kerns) - 2))
  # extract kernel parameter mu
  mu_vec = as.numeric(substr(dcs_options$kerns, nchar(dcs_options$kerns) - 1,
                             nchar(dcs_options$kerns) - 1))
                                                # extract kernel parameter mu
  
  # set kernel Function to use in optimization
  kernel_x = kernel_fcn_assign(dcs_options$kerns[1])
  kernel_t = kernel_fcn_assign(dcs_options$kerns[2])

  kernel_prop_x = kernel.prop.KR(kernel_x, k = k_vec[1])  # kernel properties R and mu_2
  kernel_prop_t = kernel.prop.KR(kernel_t, k = k_vec[2])

  # Kernels for auxiliary estimations
  kernel_kx_id = paste0(kern_type_vec[1], "_", k_vec[1] + 2, k_vec[1], 
                          k_vec[1])
  kernel_kt_id = paste0(kern_type_vec[2], "_", k_vec[2] + 2, k_vec[2], 
                          k_vec[2])
  kernel_kx = kernel_fcn_assign(kernel_kx_id)  # kernel for estimation of I_11
  kernel_kt = kernel_fcn_assign(kernel_kt_id)  # kernel for estimation of I_22
  
  h_opt = c(0.1, 0.1)                       # initial (arbitrary) values for h_0
  
  iterate = TRUE                            # iteration indicator
  iteration_count = 0
  while(iterate)                            # loop for IPI
  {
    iteration_count = iteration_count + 1
    h_opt_temp   = pmin(h_opt[1:2], c(0.45, 0.45)) 
                          # KR can't handle too large bandwidths
    h_infl  = inflation.KR(h_opt_temp, c(n_x, n_t), dcs_options)
                          # inflation of bndws for estimation of derivatives
    
    # parallel estimation of surfaces
    if (add_options$parallel == TRUE)
    {
      par_list_Y = list(h = h_opt_temp, drv = dcs_options$drv,
                        kernel_x = dcs_options$kerns[1], 
                        kernel_t = dcs_options$kerns[2])
      par_list_mxx = list(h = h_infl$h_xx,
                          drv = c(k_vec[1], dcs_options$drv[2]),
                          kernel_x = kernel_kx_id,
                          kernel_t = dcs_options$kerns[2])
      par_list_mtt = list(h = h_infl$h_tt,
                          drv = c(dcs_options$drv[1], k_vec[2]),
                          kernel_x = dcs_options$kerns[1],
                          kernel_t = kernel_kt_id)

      par_list = list(par_Y = par_list_Y, par_mxx = par_list_mxx, 
                      par_mtt = par_list_mtt)
      
      if (dcs_options$IPI_options$const_window == TRUE)
      {
        result_list = parallel.KR.const1(par_list, Y)
        Y_smth = result_list[[1]]
        mxx = result_list[[2]]
        mtt = result_list[[3]]
      } else {
        result_list = parallel.KR.const0(par_list, Y)
        Y_smth = result_list[[1]]
        mxx = result_list[[2]]
        mtt = result_list[[3]]
      }
    } else if (add_options$parallel == FALSE) {
    # constant bandwidth only reasonable for estimation of derivatives
      if (dcs_options$IPI_options$const_window == TRUE)
      {
        # pre-smoothing of the surface function m(0,0) for estimation of variance
        Y_smth = KR_dcs_const0(yMat = Y, hVec = h_opt_temp, 
                               drvVec = dcs_options$drv,
                               kernFcnPtrX = kernel_x, kernFcnPtrT = kernel_t)
        # smoothing of derivatives m(2,0) and m(0,2)
        mxx = KR_dcs_const1(yMat = Y, hVec = h_infl$h_xx,
                            drvVec = c(k_vec[1], dcs_options$drv[2]),
                            kernFcnPtrX = kernel_kx, kernFcnPtrT = kernel_t)
        mtt = KR_dcs_const1(yMat = Y, hVec = h_infl$h_tt,
                            drvVec = c(dcs_options$drv[1], k_vec[2]),
                            kernFcnPtrX = kernel_x, kernFcnPtrT = kernel_kt)
      } else if (dcs_options$IPI_options$const_window == FALSE) {
        # pre-smoothing of the surface function m(0,0) for estimation of variance
        Y_smth = KR_dcs_const0(yMat = Y, hVec = h_opt_temp,
                               drvVec = dcs_options$drv,
                               kernFcnPtrX = kernel_x,
                               kernFcnPtrT = kernel_t)
        # smoothing of derivatives m(2,0) and m(0,2)
        mxx = KR_dcs_const0(yMat = Y, hVec = h_infl$h_xx,
                            drvVec = c(k_vec[1], dcs_options$drv[2]),
                            kernFcnPtrX = kernel_kx, kernFcnPtrT = kernel_t)
        mtt = KR_dcs_const0(yMat = Y, hVec = h_infl$h_tt,
                            drvVec = c(dcs_options$drv[1], k_vec[2]),
                            kernFcnPtrX = kernel_x, kernFcnPtrT = kernel_kt)
      }
    }

    # shrink mxx, mtt from boundaries if trim > 0
    if (any(dcs_options$IPI_options$trim[1] != 0))
    {
      shrink_x = ceiling(dcs_options$IPI_options$trim[1] * n_x):
                      (n_x - floor(dcs_options$IPI_options$trim[1] * n_x))
      shrink_t = ceiling(dcs_options$IPI_options$trim[2] * n_t):
                      (n_t - floor(dcs_options$IPI_options$trim[2] * n_t))

      mxx = mxx[shrink_x, shrink_t]
      mtt = mtt[shrink_x, shrink_t]
      R = (Y - Y_smth)[shrink_x, shrink_t]
      n_sub = dim(mxx)[1]*dim(mxx)[2]   # number of used observations
    } else {
      R = Y - Y_smth
      n_sub = n                         # all observations are used
    }
    
    ### Estimation of Variance Factor and Model ###
    if (isTRUE(cf_est))
    {
      var_est = suppressWarnings(suppressMessages(cf.estimation(R, dcs_options,
                                                                add_options)))
      var_coef = var_est$cf_est
      var_model = var_est$model_est
    } else {
      var_coef = cf_est$var_coef
      var_model = cf_est$var_model
    }
      
    # calculate optimal bandwidths for next step
    if (dcs_options$var_model == "sfarima_RSS") ### Long-memory estimation
    {
      h_opt = h.opt.LM(mxx, mtt, var_coef, var_model$model, n_sub, dcs_options, 
                       n_x, n_t)
    } else {                         ### Short-memory or iid. estimation
      h_opt = h.opt.KR(mxx, mtt, var_coef, n, n_sub, k_vec, dcs_options$drv, 
                       kernel_prop_x, kernel_prop_t)
    } 
    
    # break condition
    if( ((h_opt[1]/h_opt_temp[1] - 1 < 0.001) && (h_opt[2]/h_opt_temp[2] - 1 
        < 0.001) && (iteration_count > 3)) || (iteration_count > 15) )
    {
      iterate = FALSE
    }
  }
  
  return(list(h_opt = h_opt, iterations = iteration_count, var_coef = var_coef,
              var_model = var_model))
}