# returns a vector of predictions or likelihoods
kfold_trial <- function(model, k=10, type) {
  # Grab the data and the call from the model.
  data <- data_from_mod(model)
  # For cross validation, we don't want the constructed terms
  constructed <- grep("\\(.*\\)", names(data))
  if (length(constructed) > 0) data[[constructed]] <- NULL # get rid of them
  

  fit_call <- construct_fitting_call(model, data_name = "training")
  # construct the groups for the k-fold divisions
  groups <- sample(rep(1:k, length.out = nrow(data) ))
  # Create a holder for the result
  # output <- evaluate_model(model, data = data, type = type)
  output <- numeric(nrow(data))
  
  for (group in 1:k) {
    training <- data[group != groups, , drop = FALSE ]
    
    testing  <- data[group == groups, , drop = FALSE ]
    
    this_model <- eval(fit_call)
    
    tmp <- mod_error(this_model, testdata = testing, error_type = type)
    
    output[group == groups] <- tmp
  }
  
  res = mean(output)
  names(res) <- names(tmp)[1] # name the result after the type of error actually used
  
  res
}

# helper for reference levels
# get an appropriate set of levels
n_levels <- function(values, n) {
  var_name <- substitute(values)
  n <- pmax(ceiling(abs(n)), 1)
  unique_vals <- unique(values)
  if (n == Inf) { # flag for "all levels". But don't go crazy if variable is quantitative
    res <- if (is.numeric(values)) { # enough to make a nice plot
      if (length(unique_vals) < 10) unique_vals
      else seq(min(values, na.rm = TRUE), 
               max(values, na.rm = TRUE), length = 100)
    } else {
      as.character(unique_vals) # all categorical levels
    }
    return(res)
  } 

  # finite number of categorical levels
  if ( ! is.numeric(values)) {
    level_names <- names(sort(table(values), decreasing = TRUE))
    return( level_names[1:pmin(n, length(level_names))] )
  }  
  
  # finite number of numerical levels
  if (is.numeric(values)) {
    med <- median(values, na.rm = TRUE)
    if (n == 1) {
      return(signif(med, 2))
    } else {
      res <- pretty(quantile(values, c(.1, .9)), n-1)
      if (n == 2) res <- res[c(1, length(res))] 
      if (length(res) > n) res <- res[-1]
      if (length(res) > n) res <- res[-length(res)]
      return(res)
    }
    # outliers <- has_outlier(values)
    # order_of_magnitude <- 0
    # common_digits <- range(log10(abs(unique_vals[unique_vals != 0])))
    # if (1 > diff(common_digits) && sign(min(unique_vals)) == sign(max(unique_vals))) {
    #   order_of_magnitude <- sign(max(unique_vals)) * signif(10^mean(common_digits), floor(-log10(diff(common_digits))))
    # }
    # to_two_digits <- signif(values - order_of_magnitude, 2L) 
    # 
    # trim <- ifelse(n < 10, .1, ifelse(n > 100, .01, 0.05))
    # # if no outliers, do the whole range
    # if ( ! any(outliers)) where <- seq(0, 1, length = n)
    # else if (all(outliers)) where <- seq(trim, 1-trim, length = n)
    # else if (outliers[1]) where <- seq(trim, 1, length = n)
    # else where <- seq(0, 1-trim, length = n)
    # 
    # candidate1 <- quantile(to_two_digits, where, type = 3, na.rm = TRUE)
    # 
    # return(unique(candidate1 + order_of_magnitude))
  } 

  stop("\"", var_name, "\" is neither numerical nor categorical. Can't figure out typical levels.")

}

# look for pretty extreme outliers
# return logicals: is the minimum an outlier? is the maximum an outlier?
has_outlier <- function(values, whisker = 3) {
  box <- as.numeric(quantile(values, probs = c(.25, .75)))
  c(min(values) < box[1] - diff(box) * whisker,
    max(values) > box[2] + diff(box) * whisker )
  
}


get_step = function(ref_vals, change_var, data, step = NULL, from = NULL) {
  if (is.null(step)) { # Need to set the step
    vals <- data[[change_var]]
    
    if (is.numeric(vals)){
      step <- pretty(c(.5, 1.5) * sd(vals, na.rm = TRUE), 2)[2]
    } else {
      if ( ! is.null(from)) 
        vals <- vals[ ! vals %in% from]
      else {
        vals <- vals[ ! vals %in% ref_vals[[change_var]]]
        if (length(vals) == 0) vals <- data[[change_var]]
      }
      step <- names(sort(table(vals), decreasing = TRUE))[1] 
    }
  }
  step
}

# separate ... into the components that match explanatory variables ($at) 
# and those that don't ($extras)

handle_dots_as_variables <- function(model, ...) {
  xvars <- base::union(explanatory_vars(model), names(data_from_mod(model)))
  All <- list(...)
  res <- list()
  res$at <- All[names(All) %in% xvars]
  res$extras <- All[ ! names(All) %in% xvars]
  
  res
}


