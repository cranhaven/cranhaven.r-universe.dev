#' Get Coefficients from Data
#' 
#' You need model coefficients to simulate multilevel data, and can get them from data simulated from parameters using sim_design() or rmulti().
#' 
#' @param data A dataset in long format
#' @param formula An lm formula (can be extracted from datasets created by sim_design)
#' @param fun the model function
#' @param ... Further arguments to the model function
#'
#' @return a list of the model coefficients
#' @export
#'
#' @examples
#' # simulate some data
#' data <- sim_design(within = 2, between = 2, 
#'                    mu = c(1, 0, 1, 1), 
#'                    long = TRUE, empirical = TRUE)
#' 
#' # get coefs for the full factorial model
#' get_coefs(data)
#' 
#' # a reduced model
#' get_coefs(data, y ~ B1 + W1)
#' 
#' # specify a different model function
#' data$y <- norm2binom(data$y)
#' get_coefs(data, fun = glm, family = binomial)
get_coefs <- function(data, formula = NULL, fun = stats::lm, ...) {
  if (is.null(formula)) {
    design <- get_design(data)
    factors <- c(names(design$within), names(design$between))
    txt_formula <- sprintf("%s ~ %s", 
                           names(design$dv),
                           paste(factors, collapse = " * "))
    formula <- stats::as.formula(txt_formula)
    
  }
  
  args <- list(formula = formula, data = data, ...)
  model <- do.call(fun, args)
  estimates <- model$coefficients
  terms <- attr(stats::terms.formula(formula), "term.labels")
  names(estimates) <- c("(Intercept)", terms)
  
  estimates
}
