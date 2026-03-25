#' Generate Example Data for SQUIRE
#'
#' @description
#' Creates a simple example dataset suitable for testing SQUIRE functionality.
#' The data simulates a germination experiment with three treatments.
#'
#' @param n_time Number of time points (default 8)
#' @param n_rep Number of replicates per treatment (default 4)
#' @param seed Random seed for reproducibility (default NULL)
#'
#' @return A data frame with columns: time, treatment, replicate, response
#' @importFrom stats rnorm
#' @export
#'
#' @examples
#' # Generate example data
#' example_data <- generate_example_data(seed = 123)
#' head(example_data)
#' 
#' # Check structure
#' str(example_data)
#' table(example_data$treatment, example_data$time)
#'
generate_example_data <- function(n_time = 8, n_rep = 4, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  data.frame(
    time = rep(0:(n_time-1), times = n_rep * 3),
    treatment = rep(c("Control", "Treatment_A", "Treatment_B"), each = n_time * n_rep),
    replicate = rep(rep(1:n_rep, each = n_time), times = 3),
    response = c(
      # Control
      rnorm(n_time * n_rep, 
            mean = rep(seq(0, 80, length.out = n_time), n_rep), 
            sd = 3),
      # Treatment_A (inhibited)
      rnorm(n_time * n_rep, 
            mean = rep(seq(0, 60, length.out = n_time), n_rep), 
            sd = 3),
      # Treatment_B (promoted)
      rnorm(n_time * n_rep, 
            mean = rep(seq(0, 95, length.out = n_time), n_rep), 
            sd = 3)
    )
  )
}

#' Simple Parameter Optimization Without GALAHAD
#'
#' @description
#' A simplified version of parameter optimization that doesn't require GALAHAD.
#' Used for examples and testing when GALAHAD is not available.
#'
#' @param data Data frame with time and response columns
#' @param constraints Character vector specifying constraints ("positive", "none")
#'
#' @return Named vector of parameters: rate, offset, scale
#' @export
#' 
#' @examples
#' # Create simple growth data
#' simple_data <- data.frame(
#'   time = rep(0:7, 4),
#'   response = rnorm(32, mean = rep(seq(0, 50, length.out = 8), 4), sd = 2)
#' )
#' 
#' # Optimize parameters
#' params <- optimize_simple(simple_data, constraints = "positive")
#' print(params)
#'
optimize_simple <- function(data, constraints = "positive") {
  # Define objective function
  objective <- function(params) {
    rate <- params[1]
    offset <- params[2] 
    scale <- params[3]
    
    predicted <- scale * (1 - exp(-rate * (data$time - offset)))
    predicted[data$time < offset] <- 0
    
    sum((data$response - predicted)^2)
  }
  
  # Initial parameters
  initial <- c(rate = 0.1, offset = 0, scale = max(data$response))
  
  # Apply constraints
  if (constraints == "positive") {
    result <- optim(initial, objective, 
                   method = "L-BFGS-B",
                   lower = c(0, 0, 0), 
                   upper = c(Inf, Inf, Inf))
  } else {
    result <- optim(initial, objective, method = "BFGS")
  }
  
  # Return named vector
  params <- result$par
  names(params) <- c("rate", "offset", "scale")
  return(params)
}
