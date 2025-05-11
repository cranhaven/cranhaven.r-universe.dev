formula2function <- function(mean_fun, secondary_mean_fun = NULL, data, arg_list = list()){
  m <- tryCatch(do.call(lm, args = combine_lists(arg_list[["control"]][["lm_args"]],
                                                 list(formula = mean_fun, data = data,
                                                      x = TRUE, y = TRUE, na.action = na.fail))),
                error = function(e) stop("Error fitting linear model: ", e))
  
  return(list(mean_fun = function(x, theta) sum(x*theta),
              X = m[["x"]], Y = m[["y"]],
              arg_list = arg_list))
}
