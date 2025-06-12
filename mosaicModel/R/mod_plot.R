#' Plot out model values
#'
#' @param model the model to display graphically. 
#' Can also be an ensemble produced with `mod_ensemble()`
#' @param formula setting the y ~ x + color variables
#' @param data optional data set from which to extract levels for explanatory variables
#' @param nlevels how many levels to display for those variables shown at discrete levels
#' @param at named list giving specific values at which to hold the variables. You can accomplish 
#' this without forming a list by using \code{...}. See examples.
#' @param interval show confidence or prediction intervals: values "none", "confidence", "prediction"
#' @param bootstrap when > 1, this will generate bootstrap replications of the model
#' and plot all of them. Use as an alternative to `interval` for confidence intervals.
#' @param post_transform a scalar transformation and new name for the response variable, 
#' e.g. \code{post_transform = c(price = exp)} to undo a log transformation of price.
#' @param size numerical value for line width (default: 1)
#' @param alpha numerical value in 0 to 1 for transparency (default: 0.8)
#' @param class_level character string. If a probability for a classifier is being shown,
#' which levels of the response variable to use in the plot. (Default: the first one.)
#' @param ... specific values for explantory variables 
#'
#'
#' @examples
#' \dontrun{
#' mod1 <- lm(wage ~ age * sex + sector, data = mosaicData::CPS85)
#' mod_plot(mod1)
#' mod_plot(mod1, n = Inf, interval = "confidence") 
#' mod_plot(mod1, ~ sector + sex + age) # not necessarily a good ordering
#' mod_plot(mod1, ~ age + sex + sector, nlevels = 8) 
#' mod2 <- lm(log(wage) ~ age + sex + sector, data = mosaicData::CPS85)
#' mod_plot(mod2, post_transform = c(wage = exp), 
#'      interval = "confidence") # undo the log in the display
#' mod3 <- glm(married == "Married" ~ age + sex * sector,
#'             data = mosaicData::CPS85, family = "binomial")
#' mod_plot(mod3)
#' E3 <- mod_ensemble(mod3, 10)
#' mod_plot(E3)
#' mod4 <- rpart::rpart(sector ~ age + sex + married, data = mosaicData::CPS85)
#' mod_plot(mod4)
#' mod_plot(mod4, class_level = "manag")
#' mod5 <- randomForest::randomForest(
#'          sector ~ age + sex + married, data = mosaicData::CPS85)
#' mod_plot(mod5)
#' mod_plot(mod5, class_level = "manag")
#' }
#' @export
mod_plot <- function(model=NULL, formula = NULL, 
                     data = NULL, bootstrap = 0,
                     nlevels = 3, at = list(), class_level = NULL,
                     interval = c("none", "confidence", "prediction"),
                     post_transform = NULL, size = 1, alpha = 0.8, ...) {
  
  all_models <- NULL
  if (inherits(model, "bootstrap_ensemble")) {
    orig_model <- model$original_model
  } else if (bootstrap > 1) {
    ensemble <- mod_ensemble(model, nreps = bootstrap, data = data)
    res <- 
      mod_plot(ensemble, formula = formula, data = data, bootstrap = 0, nlevels = nlevels,
               at = at, class_level = class_level, interval = "none", 
               post_transform = post_transform,
               size = size, alpha = alpha, ...)
    return(res)
    
  } else {
    orig_model <- model
  }
  
  # Deal with the arguments
  dots <- handle_dots_as_variables(orig_model, ...)
  extras <- dots$extras
  inline_values <- dots$at
      # Override the values in <at> with any set as inline arguments.
  at[names(inline_values)] <- NULL
  at <- c(at, inline_values)
  interval <- match.arg(interval)
  if (length(extras) > 0) {
    if ("intervals" %in% names(extras))
      stop("The name for the argument is \"interval\", not intervals.")
    stop("You've given some extraneous arguments. ")
  }


  # If data not explicitly provided, get from model
  levels_data <- 
    if (is.null(data)) data_from_mod(orig_model) else data
  
  # Pick out the variables to be displayed, and their roles
  explan_vars <- explanatory_vars(orig_model)
  response_var_name <- response_var(orig_model)
  
  if (is.null(formula)) show_vars <- explan_vars
  else show_vars <- all.vars(mosaic::rhs(formula))
  
  # Use the first var as <x>
  # Use the second var as <color>
  # Use the third var as <facet>
  # Use the fourth as the second <facet> variable.
  
  if (length(show_vars) > 4) show_vars <- show_vars[1:4]
  
  # Set a large number of levels for the first explanatory variable,
  # then nlevels for the remaining ones.
  how_many <- as.list(c(Inf, rep(nlevels, length(show_vars) - 1)))
  names(how_many) <- show_vars
  eval_levels <- df_typical(data = levels_data[explan_vars],
                            model = orig_model,
                            nlevels = how_many, at = at)
  
  # Evaluate the model at the specified levels
  model_vals <- mod_eval(model = model, data = eval_levels, interval = interval,
                         append = TRUE)
  
  # If it's the probability from a classifier, pick out the selected level
  if ( ! "model_output" %in% names(model_vals)) {
    if (is.null(class_level)) class_level = names(model_vals)[ncol(model_vals)] # default
     
    if (class_level %in% names(model_vals))
      model_vals$model_output <- model_vals[[class_level]]
    else
      stop("Class level \"", class_level, "\" is not in the model output.")
    
    response_var_name <- paste(response_var_name, "==", class_level)
  }
  
  # Apply the transform, if any
  if ( ! is.null(post_transform)) {
    the_transform <- post_transform[[1]]
    if ("model_output" %in% names(model_vals)) {
      model_vals[["model_output"]] <- the_transform(model_vals[["model_output"]])
    } else {
      for (column in setdiff(names(model_vals), explanatory_vars(orig_model))) {
        model_vals[[column]] <- the_transform(model_vals[[column]])
      }
    }
  }

  # if (any(names(model_vals) %in% names(eval_levels)))
  #     stop("Name conflict in output of `mod_eval()`, Eval_levels has response var name in it.")
  # model_vals <- cbind(eval_levels, model_vals)

  # figure out the components of the plot
  if (length(show_vars) > 1) {
    model_vals[[show_vars[2]]] <- as.factor(model_vals[[show_vars[2]]])
  }
  
  P <- if (length(show_vars) > 1 ) {
    ggplot(data = model_vals, 
           aes_string(x = show_vars[1], color = show_vars[2], y = "model_output"),
           group = show_vars[2])
  } else {
    ggplot(data = model_vals,
           aes_string(x = show_vars[1], y = "model_output")) 
  }
 
  first_var_quantitative <- is.numeric(eval_levels[[show_vars[1]]])
  
  if (length(show_vars) == 1) {
    if (first_var_quantitative) {
      P <- P + geom_line(size = size, alpha = alpha) 
    }
    else {
      P <- P + geom_point(size = size, alpha = alpha)
    }
  } else { # more than one explanatory variable
    if (first_var_quantitative) {
      if (is.numeric(model_vals[[show_vars[2]]]))
        for_aes <- aes_string(color = show_vars[2])
      else 
        for_aes <- aes_string(color = show_vars[2], linetype = show_vars[2])
      if (inherits(model, "bootstrap_ensemble")) {
        for (k in unique(model_vals[[".trial"]])) {
          P <- P + geom_line(data = model_vals[model_vals$.trial == k,],
                             for_aes, alpha = alpha, size = size)
        }
      } else {
        P <- P + geom_line(for_aes, alpha = alpha, size = size)
      }
    } else {
      P <- P + geom_point(
        aes_string(color = show_vars[2]), 
        alpha = alpha, size = size) 
    }
  }
  
  # The mode for confidence intervals
  if (first_var_quantitative) {
    Qfun <- geom_ribbon
  } else {
    Qfun <- geom_errorbar
  }
  
  Q <- NULL # nothing to show, unless ...
  if (interval != "none") {
    if (length(show_vars) > 1) {
      Q <- Qfun(data = model_vals, 
                aes_string(x = show_vars[1], 
                           ymax = "upper", ymin = "lower", fill = show_vars[2]), 
                color = NA, alpha = alpha/4) 
    } else {
      Q <- Qfun(data = model_vals, 
                aes_string(x = show_vars[1],
                           ymax = "upper", ymin = "lower"), 
                color = "black", alpha = alpha/4)
    }
  }
  
  
    
  if (length(show_vars) == 3)
    P <- P + facet_wrap(show_vars[3], labeller = label_both)
  if (length(show_vars) == 4)
    P <- P + facet_grid(paste(show_vars[3], "~", show_vars[4]), 
                        labeller = label_both)

  if ( ! is.null(post_transform)) response_var_name <- names(post_transform)
  P <- P + ylab(response_var_name)
  P + Q # return the plot
}
