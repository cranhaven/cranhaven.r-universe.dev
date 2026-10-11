#' A small function to change the format of the model into something easier to use
#' @keywords internal
#' @noRd
parse_model = function(model, data1, data2){
  # the formatted model
  full_model = list(
    a = a~1,
    t = t~1,
    z = z~1,
    v = v~1,
    y = NA
  )

  # number of outcomes for the GLM, currently only support 0 or 1.
  n_outcome = 0

  # the name of the outcome variable for GLM, NA if no GLM defined.
  primary_outcome = NA

  # check if there are multiple GLM outcomes and get the outcome variable name.
  for(param in model){
    outcome = all.vars(param)[[1]]
    if(!outcome %in% c("a","t","z","v")){
      n_outcome = n_outcome + 1
      primary_outcome = outcome
      full_model[["y"]] = param
    }
    else{
      full_model[[outcome]] = param
    }
    if(n_outcome >= 2){
      stop("model must contain only zero or one primary outcome")
    }
  }

  # check if primary outcome is numeric or factor
  if(!is.na(primary_outcome) && is.factor(data1[[primary_outcome]])){
    stop("primary outcome must be numeric")
  }

  # for factor variables, the original variable is replaced by dummy variables
  # this is the list of how the replacement is done for the GLM.
  glm_replacement_list = list()

  # dealing with DDM formulas
  for(param in c("a", "t", "z", "v")){
    # convert the formula to a string for editing
    formula_string = deparse(full_model[[param]])
    formula_string = paste0(formula_string, " ")

    # iterate over all variables and make the replacement
    all_variables = as.list(attr(terms(full_model[[param]]), "variables"))[-c(1,2)]
    for(variable in all_variables){
      # check if the terms in the formula is in data2
      #if(!variable %in% colnames(data2)){
      #  stop(stringr::str_interp("${variable} in model:${outcome} is not found in data2"))
      #}

      # if x is a factor variable with levels A B and C, we need to replace " x "
      # in the formula by " (xB + xC) "
      if(is.factor(data2[[variable]])){
        original_string = paste0(" ", variable ," ")
        levels_dummy = levels(data2[[variable]])[-1]
        replacement_string = paste("(", paste(paste0(variable, levels_dummy), collapse = " + "), ")")
        formula_string = stringr::str_replace_all(formula_string, original_string, replacement_string)
      }
    }

    # prepare the glm replacement list
    # for example, if "x" is replaced by "(xB + xC)" in formula of "v", then
    # "v_x" should be replaced by "(v_xB + v_xC)" in the glm formula
    all_terms = attr(terms(full_model[[param]]), "term.labels")

    # prepare a formula without outcome for model.matrix
    sub_formula = as.formula(substring(deparse(full_model[[param]]), 2))
    all_variables = colnames(model.matrix(sub_formula, data2))
    all_assign = attr(model.matrix(sub_formula, data2), "assign")
    for(i in 1:length(all_terms)){
      if(length(all_terms) == 0){
        break
      }
      replacement = all_variables[all_assign == i]
      replacement = paste0(param, "_", replacement)
      replacement = paste0(" (", paste(replacement, collapse = " + "), ") ")
      replacement = replace_colon(replacement)
      glm_replacement_list[[paste0(" ", param, "_", replace_colon(all_terms[i]), " ")]] = replacement

    }

    # transform the edited string back to a formula
    full_model[[param]] = as.formula(formula_string)

  }

  # dealing with GLM formula
  # if no glm is included, return the model
  if(is.na(primary_outcome)){
    return(full_model)
  }

  # append covariates with factors to the glm_replacement_list
  all_cov = colnames(data1)
  for(cov in all_cov){
    if(is.numeric(data1[[cov]])){
      next
    }
    replacement = levels(factor(data1[[cov]], exclude = NULL))[-1]
    replacement = paste0(cov, replacement)
    replacement = paste0(" (", paste(replacement, collapse = " + "), ") ")
    glm_replacement_list[[paste0(" ", cov, " ")]] = replacement
  }

  # convert the formula to a string for editing
  formula_string = deparse(full_model[["y"]])
  formula_string = paste0(formula_string, " ")

  # replace the variables using glm replacement list
  for(variable in names(glm_replacement_list)){
    formula_string = stringr::str_replace_all(
      formula_string,
      variable,
      glm_replacement_list[[variable]]
    )
  }

  # transform the edited string back to a formula
  full_model[["y"]] = as.formula(formula_string)

  return(full_model)
}









