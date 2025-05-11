generate_Y_mean.formula <- function(true_mean, theta, ..., true_data, true_method,
                                    true_method_args){
  if(identical(true_method, "lm")){
    true_method_args[["x"]] <- TRUE
    true_method_args[["y"]] <- TRUE
  }
  model <- do.call(get(true_method),
                   args = combine_lists(list(formula = true_mean, data = true_data),
                                        true_method_args))
  generate_Y_mean(true_mean = model, theta = theta)
}
