#' This function checks for potential problems in the data
#' @keywords internal
#' @noRd
check_data = function(
    data1,
    data2
){
  N = nrow(data1)
  n = nrow(data2)

  # check the general sample size
  if(N == 1){
    stop("Please provide at least 2 subjects for regression analysis.\n")
  }
  if(N < 5){
    warning("Less than 5 subjects provided. This may cause difficulty in model convergence.\n")
  }

  # check for id column in data1
  if(! "id" %in% colnames(data1)){
    stop("Must identify subjects using variable 'id' in data1\n")
  }
  if(length(data1$id) != length(unique(data1$id))){
    stop("'id' cannot contain duplicate values in 'data1'\n")
  }

  # check for missing values in subject-level data
  for(cov in colnames(data1)){
    if(cov == "id"){
      if(sum(is.na(dplyr::pull(data1,cov))) > 0){
        stop(stringr::str_interp("'id' column in 'data1' cannot contain missing values\n"))
      }
    }
    else{
      if(sum(!is.na(dplyr::pull(data1,cov))) < 2){
        stop(stringr::str_interp("'${cov}' in 'data1' must have at least two observations\n"))
      }
    }
  }

  # check for errors in trial-level data
  if(! "id" %in% colnames(data2)){
    stop("Must identify subjects using variable 'id' in data2\n")
  }
  if(! "response" %in% colnames(data2)){
    stop("Variable 'response' not found in data2.\n")
  }
  if(!all(unique(data2$response) %in% c(0, 1))){
    stop("Encode the 'response' of the trial in data2 using upper = 1, lower = 0\n")
  }
  if(! "rt" %in% colnames(data2)){
    stop("Variable 'rt' not found in data2. Encode the response time of subjects in seconds\n")
  }
  if(min(data2$rt) < 0.1){
    warning("Some trials has response time(rt) less than 0.1s. Did your subjects make proper decision?\n")
  }
  if(min(data2$rt) > 10){
    warning("Some trials has response time(rt) greater than 10s. Check if the unit of reaction time is in seconds.\n")
  }

  # check if all subjects in data1 and data2 matches
  for(id in unique(data2$id)){
    if(!id %in% data1$id){
      stop(paste0("Subject ", id, " in data2 has no records in data1. Check your data!\n"))
    }
  }
  for(id in data1$id){
    if(!id %in% data2$id){
      stop(paste0("Subject ", id, " in data1 has no records in data2. Check your data!\n"))
    }
  }

  # check the number of trials for each subject
  for(i in data1$id){
    if(nrow(dplyr::filter(data2, id == i)) < 10){
      warning(paste0("You have subject with less than 10 trials, which may lead to poor model performance\n"))
      break
    }
  }

  # check for missing value is data2, which is not supported
  for(variable in colnames(data2)){
    if(sum(is.na(dplyr::pull(data2, variable))) > 0){
      stop(stringr::str_interp("'${variable}' in 'data2' cannot contain missing values\n"))
    }
  }

}


#' This function checks for error in the model
#' @keywords internal
#' @noRd
check_model = function(
    xvar,
    cvar,
    model
){
  # find all valid parameters for the glm part
  valid_parameters = c(cvar,"v_0","t_0","z_0","a_0")

  # the influence of trial-level variable on ddm parameter is also valid
  for(outcome in c("a", "t", "v", "z")){
    formula = model[[outcome]]
    variable_labels = rownames(attr(terms(formula), "factors"))
    # check if trial-level variables are found in data2
    for(variable in variable_labels){
      if(variable == outcome){
        next
      }
      if(!variable %in% xvar){
        stop(stringr::str_interp("${variable} in model:${outcome} is not found in data2"))
      }
    }
    term_labels = attr(terms(formula), "term.labels")
    for(term in term_labels){
      valid_parameters = c(valid_parameters, replace_colon(paste0(outcome,"_",term)))
    }
  }

  # if no GLM is defined, no further checks
  if(!purrr::is_formula(model[["y"]])){
    return()
  }

  # check if there is duplicate parameter name
  # this could happen when there are three variables 'x1', 'x2' and 'x1_x2'
  # and a model with a ~ x1 * x2 + x1_x2 structure is fit...
  # RegDDM will generate a_x1_x2 for x1, x2 interaction terms and also a_x1_x2 for x1_x2
  # currently, there is no good solution since stan does not accept x1:x2 as a
  # variable name...
  duplicate_terms = dplyr::tibble(
    term = valid_parameters
  )
  duplicate_terms = dplyr::summarise(
    dplyr::group_by(
      duplicate_terms, by = term
    ),
    n = dplyr::n()
  )

  for(i in 1:nrow(duplicate_terms)){
    if(duplicate_terms[i,2]>1){
      stop(stringr::str_interp(
        "RegDDM generated identical name ${duplicate_terms[i,1]} for at least two parameters\n
       For example, having a variable named 'v_0' in data1 will cause this problem
       because RegDDM generates another 'v_0' for baseline drift rate\n"
      ))
    }
  }

  # check if all variables in GLM part is valid
  y_terms = as.character(attr(terms(model[["y"]]), "variables"))[-1]
  for(term in y_terms){
    if(!term %in% valid_parameters){
      stop(stringr::str_interp(
        "'${term}' is not a valid variable in formula for the outcome. See '?regddm' for reference\n"
      ))
    }
  }

}
