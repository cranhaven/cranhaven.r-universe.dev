#' This function generates a stan model specified by the user writes to a file
#' containing the model. Currently it only support Gaussian, Bernoulli and
#' Poisson distribution with canonical link function.
#' @keywords internal
#' @noRd
#'
generate_model = function(
    stan_data,
    data1,
    model,
    prior,
    family,
    file_name
){
  x_names = stan_data$x_names
  c_names = stan_data$c_names
  stan_data = stan_data$data
  primary_outcome = ifelse(purrr::is_formula(model[["y"]]), all.vars(model[["y"]])[[1]], NA)

  # make sure the outcome contains no missing data
  if(!is.na(primary_outcome) && sum(is.na(data1[[primary_outcome]] > 0))){
    stop("outcome must contain no missing data")
  }

  # make sure the data type of y matches the distribution
  if(is.na(primary_outcome)){
    TRUE # no outcome is needed thus no distribution family required
  }
  else if(family == "gaussian"){
    if(primary_outcome %in% c_names && !is.numeric(stan_data[[primary_outcome]])){
      stop(stringr::str_interp("for Gaussian family, outcome must be numeric or a DDM parameter\n"))
    }
  }
  else if(family == "bernoulli"){
    if(!primary_outcome %in% c_names){
      stop(stringr::str_interp("if outcome is a DDM parameter, gaussian family distribution is required\n"))
    }

    for(each in stan_data[[primary_outcome]]){
      if(!(each == 0 | each == 1)){
        stop(stringr::str_interp("for Bernoulli family, outcome must be either 1 or 0\n"))
      }
    }
  }
  else if(family == "poisson"){
    if(!primary_outcome %in% c_names){
      stop(stringr::str_interp("if outcome is a DDM parameter, gaussian family distribution is required\n"))
    }

    for(each in stan_data[[primary_outcome]]){
      if(as.integer(each) != each | each < 0){
        stop(stringr::str_interp("for Poisson family, outcome must be integers >= 0\n"))
      }
    }
  }
  else {
    stop(stringr::str_interp("unsupported distribution family: ${family}\n"))
  }


  # summary the information for missing data (for data1 only)
  missing_info = list()
  for(c_name in c_names){
    dat = data1[[c_name]]
    dat_unique = unique(dat)
    n_mis = sum(is.na(dat))
    n_obs = length(dat) - sum(is.na(dat))
    missing_info[[c_name]] = list(
      n_mis = n_mis,
      n_obs = n_obs,
      iid_mis = which(is.na(dat)),
      iid_obs = which(!is.na(dat))
    )
  }

  # whether to apply the default DDM priors.
  # if priors == FALSE, priors for baseline parameters modeled trial-level variables will be disabled
  # other DDM parameters are not influenced
  default_ddm_priors = list(a = TRUE, t = TRUE, z = TRUE, v = TRUE)
  if(!prior){
    for(param in c("a","t","z","v")){
      if(length(attr(terms(model[[param]]), "term.labels")) == 0){
        default_ddm_priors[[param]] = FALSE
      }
    }
  }

  # internal function to append stan code to the model file
  add_script = function(str){
    str = stringr::str_interp(str)
    write(str, file = file_name, append = TRUE)
  }


  # creating a file and start writing
  write("// RegDDM generated stan model", file = file_name, append = FALSE)

  ########
  # data #
  ########
  add_script("")
  add_script("data {")
  add_script("  int<lower = 1> N; // total number of subjects")
  add_script("  // number of missing data for each covariate")
  for(c_name in c_names){
    if(missing_info[[c_name]][['n_mis']] == 0){
      next
    }
    add_script("  int<lower = 1> N_mis_${c_name};")
    add_script("  int<lower = 1> N_obs_${c_name};")
  }
  add_script("  int<lower = 1> n; // total number of trials")

  # subject level covariates
  add_script("")
  add_script("  // subject level covariates")
  for(c_name in c_names){
    if(!is.na(primary_outcome) && c_name == primary_outcome && family %in% c("bernoulli", "poisson")){
      data_type = "int"
    }
    else{
      data_type = "real"
    }

    if(missing_info[[c_name]][['n_mis']] == 0){
      add_script("  ${data_type} ${c_name}[N];")
      next
    }
    add_script("  ${data_type} ${c_name}_obs[N_obs_${c_name}];")
    add_script("  int<lower = 0> ii_obs_${c_name}[N_obs_${c_name}];")
    if(missing_info[[c_name]][['n_mis']] == 1){
      add_script("  int<lower = 0> ii_mis_${c_name};")
    }
    else{
      add_script("  int<lower = 0> ii_mis_${c_name}[N_mis_${c_name}];")
    }
  }

  # for trial level variables
  add_script("")
  add_script("  // input for each trial")
  for(x_name in x_names){
    add_script("  vector[n] ${x_name};")
  }


  # for response, response time and subject id of each trial
  add_script("  ")
  add_script("  // response, response time and subject id of each trial")
  add_script("  int response[n];")
  add_script("  real<lower = 0> rt[n]; ")
  add_script("  int id[n];")


  add_script("}")


  ##############
  # prarmeters #
  ##############
  add_script("")
  add_script("parameters {")

  # for regression parameters
  if(!is.na(primary_outcome)){
    add_script("  // regression parameters of output")
    add_script("  real beta_0;")
    for(predictor in attr(terms(model$y), "term.labels")){
      predictor = replace_colon(predictor)
      add_script("  real beta_${predictor};")
    }
    if(family == "gaussian"){
      add_script("  real<lower = 0> sigma;")
    }
  }

  # group mean and sd of missing covariates
  add_script("")
  add_script("  // group mean and sd of covariates")
  for(c_name in c_names){
    if(!is.na(primary_outcome) && c_name == primary_outcome){
      next
    }
    add_script("  real mu_${c_name};")
    add_script("  real<lower = 0> sigma_${c_name};")

  }


  # for ddm group-level parameters
  add_script("  ")
  add_script("  // ddm group parameters")
  for(param in c("a", "t", "z", "v")){
    tmp_term = attr(terms(model[[param]]), "term.labels")
    tmp_term = c("0", tmp_term) # this is for intercept
    for(predictor in tmp_term){
      predictor = replace_colon(predictor)
      add_script("  real ${param}_${predictor}[N];")
      if(!is.na(primary_outcome) && paste0(param, "_", predictor) == primary_outcome){
        next
      }
      add_script("  real mu_${param}_${predictor};")
      add_script("  real<lower = 0> sigma_${param}_${predictor};")
    }
  }

  # missing data of covariates
  add_script("")
  add_script("  // missing covariate data")
  for(c_name in c_names){
    if(missing_info[[c_name]][['n_mis']] == 0){
      next
    }
    data_type = "real"
    if(missing_info[[c_name]][['n_mis']] == 1){
      add_script("  ${data_type} ${c_name}_mis;")
    }
    else{
      add_script("  ${data_type} ${c_name}_mis[N_mis_${c_name}];")
    }
  }

  add_script("}")

  #########################
  # transformed parameters#
  #########################
  add_script("")
  add_script("transformed parameters {")

  # combining the missing and observed covariates
  add_script("  ")
  add_script("  // combining missing and observed covariate data")
  for(c_name in c_names){
    if(missing_info[[c_name]][['n_mis']] == 0){
      next
    }
    data_type = "real"
    add_script("  ${data_type} ${c_name}[N];")
    add_script("  ${c_name}[ii_obs_${c_name}] = ${c_name}_obs;")
    add_script("  ${c_name}[ii_mis_${c_name}] = ${c_name}_mis;")
  }

  add_script("}")

  #########
  # model #
  #########
  add_script("")
  add_script("model {")

  # priors.
  # priors for ddm group parameters, adopted from HDDM
  add_script("  // priors for ddm group parameters")
  if(is.na(primary_outcome) || default_ddm_priors[["a"]] & "a_0" != primary_outcome){
  add_script("  mu_a_0 ~ gamma(1.125, 0.75);")
  add_script("  sigma_a_0 ~ normal(0, 0.1);")
  }
  if(is.na(primary_outcome) || default_ddm_priors[["t"]] & "t_0" != primary_outcome){
    add_script("  mu_t_0 ~ gamma(0.08, 0.2);")
    add_script("  sigma_t_0 ~ normal(0, 1);")
  }
  if(is.na(primary_outcome) || default_ddm_priors[["z"]] & "z_0" != primary_outcome){
    add_script("  mu_z_0 ~ normal(0.5, 0.5);")
    add_script("  sigma_z_0 ~ normal(0, 0.05);")
  }
  if(is.na(primary_outcome) || default_ddm_priors[["v"]] & "v_0" != primary_outcome){
    add_script("  mu_v_0 ~ normal(2, 3);")
    add_script("  sigma_v_0  ~ normal(0, 2);")
  }


  # modeling missing covariates
  add_script("  ")
  add_script("  // modeling covariates")
  for(c_name in c_names){
    if(!is.na(primary_outcome) && c_name == primary_outcome){
      next
    }
    add_script("  for(i in 1:N){")
    add_script("    ${c_name}[i] ~ normal(mu_${c_name}, sigma_${c_name});")
    add_script("  }")
  }


  # the prior distribution of ddm parameter for each subject when all trial-level variables are 0
  add_script("  ")
  add_script("  // priors for each subject")
  add_script("  for(i in 1:N){")

  # and for their sensitivity to changes in the conditions
  for(param in c("a", "t", "z", "v")){
    for(predictor in c("0", attr(terms(model[[param]]), "term.labels"))){
      predictor = replace_colon(predictor)
      if(!is.na(primary_outcome) && paste0(param, "_", predictor) == primary_outcome){
        next
      }
      add_script("    ${param}_${predictor}[i] ~ normal(mu_${param}_${predictor}, sigma_${param}_${predictor});")
    }
  }
  add_script("  }")

  # ddm part of the model
  add_script("  ")
  add_script("  // ddm parameter for each trial")
  add_script("  real a[n];")
  add_script("  real z[n];")
  add_script("  real t[n];")
  add_script("  real v[n];")

  add_script("  ")
  add_script("  // model RT for each trial using WFPT distribution")
  add_script("  for(j in 1:n){")
  add_script("    int i;")
  add_script("    i = id[j];")
  for(param in c("a", "z", "t", "v")){
    tmp_term = attr(terms(model[[param]]), "term.labels")
    str = stringr::str_interp("    ${param}[j] = ${param}_0[i]")
    for(predictor in tmp_term){
      tmp_str = stringr::str_c(
        stringr::str_interp("${param}_"),predictor,"[i]*",stringr::str_replace_all(predictor, ":", "[j]*")
      )
      str = stringr::str_c(str, stringr::str_interp(" + ${tmp_str}[j]"))
    }
    str = stringr::str_c(str, ";")
    str = replace_colon(str)
    add_script(str)
  }
  add_script("    ")
  add_script("    if(response[j] == 1){")
  add_script("      target += wiener_lpdf(rt[j] | a[j], t[j], z[j], v[j]);")
  add_script("    }")
  add_script("    else {")
  add_script("      target += wiener_lpdf(rt[j] | a[j], t[j], 1 - z[j], -v[j]);")
  add_script("    }")
  add_script("  }")

  # regression part of the model
  if(!is.na(primary_outcome)){
    add_script("  ")
    add_script("  // generalized linear regression part of the model")
    add_script("  real eta[N]; // linear predictor")
    add_script("  for(i in 1:N){")
    tmp_term = attr(terms(model[["y"]]), "term.labels")
    str = "    eta[i] = beta_0"
    for(predictor in tmp_term){
      tmp_str = stringr::str_c(
        "beta_",predictor,"*",stringr::str_replace_all(predictor, ":", "[i]*")
      )
      str = stringr::str_c(str, stringr::str_interp(" + ${tmp_str}[i]"))
    }
    str = stringr::str_c(str, ";")
    str = replace_colon(str)
    add_script(str)
    if(family == "gaussian"){
      add_script("    ${primary_outcome}[i] ~ normal(eta[i], sigma);")
    }
    else if(family == "bernoulli"){
      add_script("    ${primary_outcome}[i] ~ bernoulli(inv_logit(eta[i]));")
    }
    else if(family == "poisson"){
      add_script("    ${primary_outcome}[i] ~ poisson(exp(eta[i]));")
    }
    add_script("  }")
  }

  add_script("}")
}
