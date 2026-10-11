#' This function generates the default initialization list for RStan
#' Because DDM parameters have certain constraints, default initialization of
#' Rstan may result in Initilization failure.
#' RegDDM will adopt a smarter way. The user can also provide other inits.
#' @keywords internal
#' @noRd
generate_default_initialization = function(data1, data2, model){
  primary_outcome = ifelse(purrr::is_formula(model[["y"]]), all.vars(model[["y"]])[[1]], NA)
  outcome_type = ifelse(primary_outcome %in% colnames(data1), 1, 2) # 1: subject-level variable, 2: DDM parameter

  init_list = list()

  ddm_init_mean = list(a_0 = 1, z_0 = 0.5, v_0 = 0, t_0 = 0.2)
  ddm_init_sd = list(a_0 = 0.5, z_0 = 0.2, v_0 = 0.5, t_0 = 0.1)

  # Subject-level baseline DDM parameters starts from reasonable values.
  N = nrow(data1)
  init_list[["a_0"]] = rep(ddm_init_mean[["a_0"]], N)
  init_list[["z_0"]] = rep(ddm_init_mean[["z_0"]], N)
  init_list[["v_0"]] = rep(ddm_init_mean[["v_0"]], N)

  # Note that baseline non-decision time needs special treatment.
  # They start from half of the minimal reaction time of each subject.
  init_list[["t_0"]] = rep(NA, N)
  min_reaction_time = dplyr::summarise(dplyr::group_by(data2, .data$id), rt = min(.data$rt))
  for(i in 1:N){
    init_list[["t_0"]][i] = as.numeric(min_reaction_time[i,2]/2)
  }

  # Initialize other subject-level ddm parameters to 0.
  # Group mean and SD are initialized to 1 and 0 respectively.
  for(param in c("a", "t", "z", "v")){
    tmp_term = attr(terms(model[[param]]), "term.labels")
    for(predictor in tmp_term){
      predictor = replace_colon(predictor)
      init_list[[stringr::str_interp("mu_${param}_${predictor}")]] = 0
      init_list[[stringr::str_interp("sigma_${param}_${predictor}")]] = 1
      init_list[[stringr::str_interp("${param}_${predictor}")]] = rep(0, N)
    }
  }

  ddm_init_mean[["t_0"]] = mean(init_list[["t_0"]])
  ddm_init_sd[["t_0"]] = sd(init_list[["t_0"]])

  # For GLM regression coefficients, start from no correlation.
  # The intercept is the mean of output and all slopes are 0.
  # Standard deviation (SD) of error term is the SD of output.
  if(!is.na(primary_outcome) && outcome_type == 1){
    init_list[["beta_0"]] = mean(data1[[primary_outcome]])
    init_list[["sigma"]] = sd(data1[[primary_outcome]])
    for(predictor in attr(terms(model$y), "term.labels")){
      predictor = replace_colon(predictor)
      init_list[[paste0("beta_", predictor)]] = 0
    }
  }
  else if(!is.na(primary_outcome) && outcome_type == 2){
    if(primary_outcome %in% c("a_0", "t_0", "z_0", "v_0")){
      init_list[["beta_0"]] = ddm_init_mean[[primary_outcome]]
      init_list[["sigma"]] = ddm_init_sd[[primary_outcome]]
    }
    else{
      init_list[["beta_0"]] = 0
      init_list[["sigma"]] = 1
    }

    for(predictor in attr(terms(model$y), "term.labels")){
      predictor = replace_colon(predictor)
      init_list[[paste0("beta_", predictor)]] = 0
    }
  }

  # Group mean and SD of baseline DDM parameters starts from some reasonable values.
  for(param in c("a_0", "t_0", "z_0", "v_0")){
    if(!is.na(primary_outcome) && param == primary_outcome){
      next
    }
    init_list[[paste0("mu_", param)]] = ddm_init_mean[[param]]
    init_list[[paste0("sigma_", param)]] = ddm_init_sd[[param]]
  }

  return(init_list)
}
