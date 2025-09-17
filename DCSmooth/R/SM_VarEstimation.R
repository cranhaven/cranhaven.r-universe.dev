################################################################################
#                                                                              #
#                DCSmooth Package: estimation of cf coefficients               #
#                                                                              #
################################################################################

# Estimation of the cf coefficient for bandwidth selection

# cf.estimation
  # cf.from.model

# Y should be the residuals, e.g. Y - YSmth in the estimation codes
cf.estimation = function(Y, dcs_options, add_options)
{
  if (!is.list(add_options$model_order) && length(add_options$model_order) == 1)
  {
    if (dcs_options$var_model == "sfarima_RSS")
    {
      model_order = sfarima.ord(Y, pmax = add_options$order_max$ar,
                                qmax = add_options$order_max$ma, 
                                crit = add_options$model_order, restr = NULL,
                                sFUN = min, parallel = add_options$parallel)
    } else {
      model_order = sarma.order(Y, method = "sep",
                                criterion = add_options$model_order,
                                order_max = add_options$order_max,
                                parallel = add_options$parallel)
    }
  } else {
    model_order = add_options$model_order
  }
  
  if (dcs_options$var_model == "iid")
  {
    cf_est = stats::sd(Y)^2
    model_est = list(model = list(sigma = stats::sd(Y)), stnry = TRUE)
  } else if (dcs_options$var_model == "sarma_HR") {
    sarma_HR = sarma.HR.est(Y, model_order = model_order)
    cf_est = cf.from.model(sarma_HR$model)
    model_est = sarma_HR
  } else if (dcs_options$var_model == "np") {
    cf_est = specDens(Y, omega = c(0, 0))$cf
    model_est = list(stnry = TRUE)
  } else if (dcs_options$var_model == "sarma_sep") {
    sarma_sep = sarma.sep.est(Y, model_order = model_order)
    cf_est = cf.from.model(sarma_sep$model)
    model_est = sarma_sep
  } else if (dcs_options$var_model == "sarma_RSS") {
    sarma_RSS = sarma.RSS.est(Y, model_order = model_order)
    cf_est = cf.from.model(sarma_RSS$model)
    model_est = sarma_RSS
  } else if (dcs_options$var_model == "sfarima_RSS") {
    sfarima = sfarima.cf(Y, model_order = model_order)
    cf_est = sfarima$cf
    model_est = sfarima$var_model
  }
  
  if (is.na(cf_est))
  {
    stop("Non-finite variance estimated. Check input matrix Y.")
  }
  
  return(list(cf_est = cf_est, model_est = model_est))
}

# Calculate c_f from estimated model

cf.from.model = function(sarma_model)
{
  sum(sarma_model$ma)^2/sum(sarma_model$ar)^2 * sarma_model$sigma^2
}