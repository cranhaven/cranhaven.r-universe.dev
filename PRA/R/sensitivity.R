#' Sensitivity Analysis.
#'
#' This function performs sensitivity analysis on a project with multiple tasks,
#' each having its own cost distribution. It calculates the sensitivity of the
#' variance in total project cost with respect to the variance in each task's cost.
#' It can also account for correlations between task costs if a correlation matrix
#' is provided.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on dimensions - validates cor_mat dimensions match number of tasks.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure.*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.list() and is.matrix() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G3.1} *Correlation handling is user-controlled via optional cor_mat parameter.*
#' @srrstats {G3.1a} *Documentation describes usage of correlation matrix in examples.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param task_dists A list of lists describing each task distribution. Each inner
#' list should contain the type of distribution and its parameters. Supported distributions
#' are "normal", "triangular", and "uniform".
#' @param cor_mat The correlation matrix for the tasks (Optional). If provided, it
#' should be a square matrix with dimensions equal to the number of tasks. If not
#' provided, tasks are assumed to be independent.
#' @return The function returns a vector of sensitivity results with respect to
#' each task. Each element in the vector corresponds to the sensitivity of the variance
#' in total project cost with respect to the variance in the respective task's cost.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' # Set the task distributions for a toy project.
#' task_dists <- list(
#'   list(type = "normal", mean = 10, sd = 2), # Task A: Normal distribution
#'   list(type = "triangular", a = 5, b = 15, c = 10), # Task B: Triangular distribution
#'   list(type = "uniform", min = 8, max = 12) # Task C: Uniform distribution
#' )
#'
#' # Set the correlation matrix between the tasks.
#' cor_mat <- matrix(c(
#'   1, 0.5, 0.3,
#'   0.5, 1, 0.4,
#'   0.3, 0.4, 1
#' ), nrow = 3, byrow = TRUE)
#'
#' # Calculate the sensitivity of each task and print the results
#' sensitivity_results <- sensitivity(task_dists, cor_mat)
#' print(sensitivity_results)
#'
#' # Build a vertical barchart and display the results.
#' data <- data.frame(
#'   Tasks = c("A", "B", "C"),
#'   Sensitivity = sensitivity_results
#' )
#' barplot(
#'   height = data$Sensitivity, names = data$Tasks, col = "skyblue",
#'   horiz = TRUE, xlab = "Sensitivity", ylab = "Tasks"
#' )
#' title("Sensitivity Analysis of Project Tasks")
#'
#' # Without correlation matrix
#' sensitivity_results_indep <- sensitivity(task_dists)
#' print(sensitivity_results_indep)
#'
#' # Build a vertical barchart and display the results.
#' data_indep <- data.frame(
#'   Tasks = c("A", "B", "C"),
#'   Sensitivity = sensitivity_results_indep
#' )
#' barplot(
#'   height = data_indep$Sensitivity, names = data_indep$Tasks,
#'   col = "lightgreen",
#'   horiz = TRUE, xlab = "Sensitivity", ylab = "Tasks"
#' )
#' title("Sensitivity Analysis of Project Tasks (Independent)")
#'
#' @export
# Define the sensitivity analysis function
sensitivity <- function(task_dists, cor_mat = NULL) {
  # Error handling
  if (is.null(task_dists)) {
    stop("task_dists must not be NULL")
  }
  if (!is.list(task_dists) || length(task_dists) == 0) {
    stop("task_dists must be a non-empty list")
  }

  num_tasks <- length(task_dists)

  # Extract variances from task distributions
  task_variances <- sapply(task_dists, function(dist) {
    if (dist$type == "normal") {
      return(dist$sd^2)
    } else if (dist$type == "triangular") {
      return((dist$a^2 + dist$b^2 + dist$c^2 - dist$a * dist$b - dist$a * dist$c - dist$b * dist$c) / 18)
    } else if (dist$type == "uniform") {
      return((dist$max - dist$min)^2 / 12)
    } else {
      stop("Unsupported distribution type.")
    }
  })

  # Create the covariance matrix
  if (!is.null(cor_mat)) {
    if (!is.matrix(cor_mat) || nrow(cor_mat) != num_tasks || ncol(cor_mat) != num_tasks) {
      stop("The correlation matrix must be square and match the number of tasks.")
    }
    if (any(is.nan(cor_mat))) {
      stop("cor_mat must not contain NaN values")
    }
    if (anyNA(cor_mat)) {
      stop("cor_mat must not contain NA values")
    }
    if (any(is.infinite(cor_mat))) {
      stop("cor_mat must not contain infinite values")
    }

    cov_matrix <- matrix(0, nrow = num_tasks, ncol = num_tasks)
    for (i in seq_len(num_tasks)) {
      for (j in seq_len(num_tasks)) {
        cov_matrix[i, j] <- cor_mat[i, j] * sqrt(task_variances[i] * task_variances[j])
      }
    }
  } else {
    cov_matrix <- diag(task_variances)
  }

  # Calculate the total variance of the project cost
  total_variance <- sum(task_variances) + sum(cov_matrix[upper.tri(cov_matrix)] * 2)

  # Initialize sensitivity vector
  sensitivity <- numeric(num_tasks)

  # Calculate the sensitivity of the total variance with respect to each task's variance
  for (i in seq_len(num_tasks)) {
    sensitivity[i] <- 1 + 2 * sum(cov_matrix[i, -i] / sqrt(task_variances[i] * task_variances[-i]))
  }

  # Return the sensitivity vector
  return(sensitivity)
}
