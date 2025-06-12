#' Calculate effect sizes in a model
#'
#' Like a derivative or finite-difference

#' @param model the model from which the effect size is to be calculated
#' @param formula a formula whose right-hand side is the variable with respect
#' to which the effect size is to be calculated.
#' @param step the numerical stepsize for the change var, or a comparison category
#' for a categorical change var. This will be either a character string or a number,
#' depending on the type of variable specified in the formula.
#' @param bootstrap The number of bootstrap replications to construct. 
#' If greater than 1, calculate a standard error using that number of replications.
#' @param to a synonym for step. (In English, "to" is more appropriate for a 
#' categorical input, "step" for a quantitative. But you can use either.)
#' @param nlevels integer specifying the number of levels to use for "typical" inputs. (Default: up to 3)
#' @param ... additional arguments for evaluation levels of explanatory 
#' variables.
#' @param at similar to \code{...} but expects a list or dataframe of the values you want to set.
#' Like \code{...}, all combinations of the values specified will be used as inputs.
#' @param data Specifies exactly the cases at which you want to calculate the effect size.
#' @param class_level Name of the categorical level for which the probability is to be used. Applies 
#' only to classifiers. (Default: Use the first level.)
#' Unlike \code{...} or \code{at}, no new combinations will be created.
#' 
#' @return a data frame giving the effect size and the values of the explanatory variables at which
#' the effect size was calculated. There will also be a column `to_` showing the value jumped to for the 
#' variable with respect to which the effect size is calculated. When `bootstrap` is greater than 1, there will
#' be a standard error reported on the effect size; see the variable ending in `_se`.
#' @details 
#' When you want to force or restrict the effect size calculation to specific values for 
#' explanatory variables, list those variables and levels as a vector in ...
#' For example, \code{educ = c(10, 12, 16)} will cause the effect size to be calculated
#' at each of those three levels of education. Any variables whose levels are not specified in 
#' ... will have values selected automatically.
#'
#' @examples
#' mod1 <- lm(wage ~ age * sex * educ + sector, data = mosaicData::CPS85)
#' mod_effect(mod1, ~ sex)
#' mod_effect(mod1, ~ sector)
#' mod_effect(mod1, ~ age, sex = "M", educ = c(10, 12, 16), age = c(30, 40))
#' mod_effect(mod1, ~ age, sex = "F", age = 34, step = 1)
#' mod_effect(mod1, ~ sex, age = 35, sex = "M", to = "F" )
#' # For classifiers, the change in *probability* of a level is reported.
#' mod2 <- rpart::rpart(sector ~ age + sex + educ + wage, data = mosaicData::CPS85)
#' mod_effect(mod2, ~ educ)
#' mod_effect(mod2, ~ educ, class_level = "manag")

#' @export
mod_effect <- function(model, formula, step = NULL, 
                        bootstrap = 0, to = step, nlevels = 1, 
                       data = NULL, at = NULL, class_level = NULL, ... ) {
  
  if (inherits(model, "bootstrap_ensemble")) {
    ensemble <- model$replications # that is, the list of bootstrapped models
    original_model <- model$original_model
    ensemble_flag  <- TRUE
  } else if (bootstrap > 1) {
    ensemble <- mod_ensemble(model, bootstrap)
    Bootstrap_reps <- 
      mod_effect(ensemble, formula, at = at, nlevels = nlevels, 
                 step = step, to = to, data = data, 
                 class_level = class_level, ...)
    res <- Bootstrap_reps %>% 
      dplyr::group_by(.trial) %>% 
      dplyr::mutate(.row = row_number()) %>%
      dplyr::ungroup() %>% 
      dplyr::group_by(.row) 
    effect_name <- names(res)[1]
    names(res)[1] <- "slope"
    res <- res %>%
      dplyr::summarise(.slope = mean(slope, na.rm = TRUE),
                .slope_se = sd(slope, na.rm = TRUE)) %>% 
      dplyr::select(-.row) 
    names(res) <- paste0(effect_name, c("_mean", "_se"))
    explanatory_vals <- Bootstrap_reps[,-1] %>% dplyr::filter(.trial == 1) %>%
      dplyr::select(-.trial)
    return(bind_cols(res, explanatory_vals))        
  } else {
    ensemble <- list(model) # Just the one model to be evaluated
    original_model <- model
    ensemble_flag <- FALSE
  }

  dots <- handle_dots_as_variables(original_model, ...)
  inline_vals <- dots$at
  # combine values set inline with those set in <at>, overriding the ones in <at>
  at[names(inline_vals)] <- NULL
  at <- c(at, inline_vals)
  extras <- dots$extras
  
  
  change_var <- all.vars(mosaic::rhs(formula))[1]
  # set up so that glms are evaluated, by default, as the response rather than the link
  if (inherits(model, "glm") && (! "type" %in% names(extras))) {
    extras$type = "response"
  }
  if (length(at) > 0 && !is.null(data)) {
    warning("You specified 'at' levels. Using these instead of raw data.") 
  }
  from_inputs <- to_inputs <- 
    if (is.null(data) || length(at) > 0) {
      data <- data_from_mod(original_model)
      df_typical(model = original_model, data = data, nlevels = nlevels, at = at)
    } else {
      # if data explicitly stated, use exactly those inputs
      data
    }
  step <- get_step(from_inputs, change_var, data, step = unlist(to))
  
  # deal with the architectures written only for factors by
  # converting the step to a factor
  if (inherits(from_inputs[[change_var]], "factor")) {
    levels_for_change_var <- unique(as.character(data[[change_var]]))
    from_inputs[[change_var]] = factor(from_inputs[[change_var]], 
                                       levels = levels_for_change_var)
    step <- factor(step, levels = levels_for_change_var)
  }
  # construct inputs for step from baseline
  if (is.numeric(step)) {
    to_inputs[[change_var]] <- from_inputs[[change_var]] + step
  } else {
    to_inputs[[change_var]] <- step
  }
  
  # Create the output data frame with vars in desired order.
  # change_var and to:change_var should be first two columns

  output_form <- from_inputs
  output_form[[change_var]] <- NULL # remove it temporarily
  output_form <- rev(output_form) 
  output_form[[paste0("to_", change_var)]] <- to_inputs[[change_var]]
  output_form[[change_var]] <- from_inputs[[change_var]]
  output_form <- rev(output_form) 
  
  # set up for accumulated output 

  Result <- NULL
  # if the "model" input was a single model, rather than an ensemble,
  # that single model is being stored as an ensemble of 1, so that
  # the same loop covers both cases.
  output_column <- 1
  output_name_append <- ""
  for (k in 1:length(ensemble)) {
    base_vals <- mod_eval(ensemble[[k]], data = from_inputs, append = FALSE)
    
    # model output after the step (or "offset")
    offset_vals <- mod_eval(ensemble[[k]], data = to_inputs, append = FALSE)
    if (k == 1 &&  ! "model_output" %in% names(base_vals)) {
      # it's a classifier
      if (!is.null(class_level)) {
        if ( ! class_level %in% names(base_vals)) {
          warning("\"", class_level, "\" is not a valid level for the response variable. Will be using ", 
                  names(base_vals)[output_column], " instead.")
        } else {
          output_column <- which(class_level == names(base_vals))
        }
      }
      output_name_append <- paste0("_", names(base_vals)[output_column])
    }
    res <- if (is.numeric(step)) {
      data.frame(slope =  (offset_vals[[output_column]] - base_vals[[output_column]]) / step)
    } else {
      data.frame(change = offset_vals[[output_column]] - base_vals[[output_column]])
    }
    if (ensemble_flag) output_form$.trial <- k
    
    Result <- rbind(Result, cbind(res, output_form))
  }


  # If a classifier, show the name of the level whose probability is being shown
  names(Result)[1] <- paste0(names(Result)[1], output_name_append)

  Result
}  
  
