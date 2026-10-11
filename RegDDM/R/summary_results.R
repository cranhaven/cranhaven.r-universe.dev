#' This function summarizes the stan fit according to the regddm model structure
#' Making the results more tidy and easier to use.
#' @keywords internal
#' @noRd
summary_results = function(stan_fit,data1, ...){
  # use default rstan function to summary the results
  res = as.data.frame(rstan::summary(stan_fit, ...)$summary)
  statistics_list = colnames(res)
  res$variable = rownames(res)
  res = dplyr::tibble(res)
  res = dplyr::select(res, "variable", dplyr::everything())

  # This local function extracts a certain statistics of all subjects' DDM
  # parameters.
  extract_subject_ddm = function(statistics){
    subject_ddm =
      dplyr::filter(res, stringr::str_detect(.data$variable, "^[atzv]_.+\\[\\d+\\]$"))

    subject_ddm =
      dplyr::mutate(
        subject_ddm,
        variable = stringr::str_remove(.data$variable, "\\[\\d+\\]"),
        id = rep(data1$id,times = nrow(subject_ddm)/nrow(data1))
      )
    subject_ddm =
      dplyr::select(subject_ddm, "id", !!rlang::sym(statistics), "variable")
    subject_ddm =
      tidyr::pivot_wider(subject_ddm, names_from = .data$variable, values_from = !!rlang::sym(statistics))

    return(subject_ddm)
  }

  # for regression coefficients
  glm_coefficiets =
    dplyr::filter(res, stringr::str_detect(.data$variable, "^beta_") | stringr::str_detect(.data$variable, "^sigma$"))

  # for subject-level ddm parameters
  subject_ddm_param = list()
  for(statistic in statistics_list){
    subject_ddm_param[[statistic]] = extract_subject_ddm(statistic)
  }

  # for group mean and sd of subjects' DDM parameters and covarites.
  group_param = dplyr::filter(
    res,
    stringr::str_detect(.data$variable,"^(mu|sigma)_.+$")
  )

  # for estimated missing values
  missing_value = dplyr::filter(
    res,
    stringr::str_detect(.data$variable,"^.+_mis.*$")
  )

  max_rhat = max(res$Rhat, na.rm = TRUE) # remove NaN. Some variables may be fixed

  return(
    list(
      glm_coefficiets = glm_coefficiets, # GLM regression parameters
      subject_ddm_param = subject_ddm_param, # ddm parameters of each subject
      group_param = group_param, # group mean and SD of DDM parameters
      missing_value = missing_value, # estimated missing covariates
      max_rhat = max_rhat # maximum r-hat statistics measuring convergence
    )
  )
}



