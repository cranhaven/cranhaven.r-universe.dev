#' Resource-based 'Parent' Design Structure Matrix (DSM).
#'
#' This function computes the Resource-based 'Parent' Design Structure Matrix (DSM)
#' from a given Resource-Task Matrix 'S'. The 'Parent' DSM indicates the number of resources
#' shared between each pair of tasks in a project.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on dimensions and types of matrix input.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure (resources x tasks matrix).*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.matrix(), is.data.frame(), and is.numeric() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected (matrix or data frame).*
#' @srrstats {G2.7} *Accepts both matrix and data.frame as input.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param S Resource-Task Matrix 'S' giving the links (arcs) between resources and tasks.
#' Rows represent resources and columns represent tasks.
#' @return An S3 object of class `"dsm"` with the following components:
#' \describe{
#'   \item{matrix}{The Resource-based 'Parent' DSM giving the number of resources shared between each task.}
#'   \item{type}{Character string `"parent"`.}
#'   \item{n_tasks}{Number of tasks (columns in S).}
#'   \item{n_resources}{Number of resources (rows in S).}
#' }
#' @references
#' Govan, Paul, and Ivan Damnjanovic. "The resource-based view on project risk management."
#' Journal of construction engineering and management 142.9 (2016): 04016034.
#' @examples
#' # Set the S matrix for a toy project (3 resources x 4 tasks).
#' s <- matrix(c(1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1), nrow = 3, ncol = 4)
#' cat("Resource-Task Matrix:\n")
#' print(s)
#'
#' # Calculate the Resource-based Parent DSM and print the results.
#' resource_dsm <- parent_dsm(s)
#' print(resource_dsm)
#'
#' @export
# Parent DSM function
parent_dsm <- function(S) {
  # Error handling
  if (is.null(S)) {
    stop("S must not be NULL")
  }
  if (!is.matrix(S) && !is.data.frame(S)) {
    stop("S must be a matrix or data frame")
  }
  if (!is.numeric(as.matrix(S))) {
    stop("S must contain numeric values")
  }
  if (any(is.nan(as.matrix(S)))) {
    stop("S must not contain NaN values")
  }
  if (anyNA(as.matrix(S))) {
    stop("S must not contain NA values")
  }
  if (any(is.infinite(as.matrix(S)))) {
    stop("S must not contain infinite values")
  }

  S <- as.matrix(S)

  # Compute the tasks x tasks DSM: t(S) %*% S
  dsm_matrix <- t(S) %*% S

  result <- list(
    matrix = dsm_matrix,
    type = "parent",
    n_tasks = ncol(S),
    n_resources = nrow(S)
  )
  class(result) <- "dsm"
  return(result)
}


#' Risk-based 'Grandparent' Design Structure Matrix (DSM).
#'
#' This function computes the Risk-based 'Grandparent' Design Structure Matrix (DSM)
#' from given Resource-Task Matrix 'S' and Risk-Resource Matrix 'R'.
#' The 'Grandparent' DSM indicates the number of risks shared between each pair of
#' tasks in a project.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on dimensions of matrix inputs - validates compatible dimensions for multiplication.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure (resources x tasks for S, risks x resources for R).*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.matrix(), is.data.frame(), and is.numeric() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected (matrix or data frame).*
#' @srrstats {G2.7} *Accepts both matrix and data.frame as input.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param S Resource-Task Matrix 'S' giving the links (arcs) between resources and tasks.
#' Rows represent resources and columns represent tasks.
#' @param R Risk-Resource Matrix 'R' giving the links (arcs) between risks and resources.
#' Rows represent risks and columns represent resources.
#' @return An S3 object of class `"dsm"` with the following components:
#' \describe{
#'   \item{matrix}{The Risk-based 'Grandparent' DSM giving the number of risks shared between each task.}
#'   \item{type}{Character string `"grandparent"`.}
#'   \item{n_tasks}{Number of tasks (columns in S).}
#'   \item{n_resources}{Number of resources (rows in S).}
#'   \item{n_risks}{Number of risks (rows in R).}
#' }
#' @references
#' Govan, Paul, and Ivan Damnjanovic. "The resource-based view on project risk management."
#' Journal of construction engineering and management 142.9 (2016): 04016034.
#' @examples
#' # Set the S and R matrices and print the results.
#' S <- matrix(c(1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1), nrow = 3, ncol = 4)
#' R <- matrix(c(1, 1, 0, 1, 0, 0), nrow = 2, ncol = 3)
#' cat("Resource-Task Matrix (3 resources x 4 tasks):\n")
#' print(S)
#' cat("\nRisk-Resource Matrix (2 risks x 3 resources):\n")
#' print(R)
#' # Calculate the Risk-based Grandparent Matrix and print the results.
#' risk_dsm <- grandparent_dsm(S, R)
#' print(risk_dsm)
#'
#' @export

# Grandparent DSM function
grandparent_dsm <- function(S, R) {
  # Error handling
  if (is.null(S) || is.null(R)) {
    stop("S and R must not be NULL")
  }
  if (!is.matrix(S) && !is.data.frame(S)) {
    stop("S must be a matrix or data frame")
  }
  if (!is.matrix(R) && !is.data.frame(R)) {
    stop("R must be a matrix or data frame")
  }
  if (!is.numeric(as.matrix(S)) || !is.numeric(as.matrix(R))) {
    stop("S and R must contain numeric values")
  }
  if (any(is.nan(as.matrix(S))) || any(is.nan(as.matrix(R)))) {
    stop("S and R must not contain NaN values")
  }
  if (anyNA(as.matrix(S)) || anyNA(as.matrix(R))) {
    stop("S and R must not contain NA values")
  }
  if (any(is.infinite(as.matrix(S))) || any(is.infinite(as.matrix(R)))) {
    stop("S and R must not contain infinite values")
  }

  S <- as.matrix(S)
  R <- as.matrix(R)

  # Check dimension compatibility: R (risks x resources) %*% S (resources x tasks)
  # requires ncol(R) == nrow(S)
  if (nrow(S) != ncol(R)) {
    stop("Number of rows in S (resources) must equal the number of columns in R (resources).")
  }

  # Compute the risk-task matrix: R %*% S (risks x tasks)
  risk_task <- R %*% S

  # Compute the tasks x tasks DSM: t(risk_task) %*% risk_task
  dsm_matrix <- t(risk_task) %*% risk_task

  result <- list(
    matrix = dsm_matrix,
    type = "grandparent",
    n_tasks = ncol(S),
    n_resources = nrow(S),
    n_risks = nrow(R)
  )
  class(result) <- "dsm"
  return(result)
}


#' Print a DSM object.
#'
#' @param x A `dsm` object returned by [parent_dsm()] or [grandparent_dsm()].
#' @param ... Additional arguments passed to [print.default()].
#' @return Invisibly returns `x`.
#' @export
#' @method print dsm
print.dsm <- function(x, ...) {
  type_label <- if (x$type == "parent") {
    "Resource-based 'Parent'"
  } else {
    "Risk-based 'Grandparent'"
  }
  cat(type_label, "Design Structure Matrix\n")
  cat("Tasks:", x$n_tasks, " Resources:", x$n_resources)
  if (!is.null(x$n_risks)) cat("  Risks:", x$n_risks)
  cat("\n\n")
  print(x$matrix, ...)
  invisible(x)
}


#' Plot a DSM heatmap.
#'
#' Displays the Design Structure Matrix as a heatmap where color intensity
#' represents the number of shared resources (parent) or risks (grandparent)
#' between task pairs.
#'
#' @param x A `dsm` object returned by [parent_dsm()] or [grandparent_dsm()].
#' @param main Optional plot title. If `NULL`, a default title is generated.
#' @param col Color palette vector. If `NULL`, uses [grDevices::heat.colors()].
#' @param ... Additional arguments passed to [graphics::image()].
#' @return Invisibly returns `x`.
#' @importFrom graphics image axis
#' @importFrom grDevices heat.colors
#' @export
#' @method plot dsm
plot.dsm <- function(x, main = NULL, col = NULL, ...) {
  m <- x$matrix
  n <- nrow(m)
  if (n == 0) {
    message("Nothing to plot: DSM has zero dimensions.")
    return(invisible(x))
  }
  if (is.null(main)) {
    type_label <- if (x$type == "parent") "Parent" else "Grandparent"
    main <- paste(type_label, "DSM")
  }
  if (is.null(col)) {
    col <- grDevices::heat.colors(12, rev = TRUE)
  }
  labels <- colnames(m)
  if (is.null(labels)) labels <- seq_len(n)
  # image() plots bottom-to-top, so reverse row order for conventional DSM layout
  graphics::image(seq_len(n), seq_len(n), t(m[n:1, , drop = FALSE]),
                  main = main, xlab = "Task", ylab = "Task",
                  axes = FALSE, col = col, ...)
  graphics::axis(1, at = seq_len(n), labels = labels)
  graphics::axis(2, at = seq_len(n), labels = rev(labels))
  invisible(x)
}
