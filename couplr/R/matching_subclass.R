# ==============================================================================
# Subclassification on Propensity Score
# ==============================================================================

#' Subclassification on Propensity Score
#'
#' Divides units into K strata based on quantiles of the propensity score,
#' then computes within-stratum weights for treatment effect estimation.
#' This is a simple, transparent approach to propensity score adjustment
#' that allows visual inspection of balance within each subclass.
#'
#' @param formula Formula for propensity score model (e.g.,
#'   \code{treatment ~ age + income}). Ignored if \code{ps} is provided.
#' @param data Data frame containing all variables
#' @param treatment Character, name of the binary treatment column (0/1)
#' @param n_subclasses Integer, number of subclasses to create (default: 5).
#'   Cochran (1968) showed that 5 subclasses removes over 90\% of bias from
#'   a single covariate.
#' @param ps Optional pre-computed numeric vector of propensity scores
#'   (one per row in \code{data}). If NULL, a logistic regression model is
#'   fit using \code{formula}.
#' @param ps_model Optional pre-fitted \code{glm} object for propensity scores
#' @param estimand Target estimand: \code{"ATT"} (default), \code{"ATE"}, or
#'   \code{"ATC"}
#'
#' @return An S3 object of class \code{c("subclass_result", "couplr_result")}
#'   containing:
#' \describe{
#'   \item{matched}{Tibble with columns \code{id}, \code{side}, \code{subclass},
#'     \code{ps}, \code{weight}}
#'   \item{subclass_summary}{Tibble with per-subclass statistics: counts,
#'     mean PS, and overlap status}
#'   \item{info}{List with \code{n_subclasses}, \code{estimand}, \code{n_left},
#'     \code{n_right}, \code{method}, \code{vars}}
#' }
#'
#' @details
#' The algorithm:
#' \enumerate{
#'   \item Estimate propensity scores via logistic regression (or use
#'     pre-computed scores)
#'   \item Divide the propensity score distribution into K quantile-based strata
#'   \item For each stratum, check overlap (both treated and control units
#'     present)
#'   \item Compute within-stratum weights based on the target estimand:
#'     \itemize{
#'       \item \strong{ATT}: Treated units get weight 1; control units get
#'         weight \code{n_treated_in_stratum / n_control_in_stratum}
#'       \item \strong{ATE}: Both groups get weight proportional to stratum
#'         size relative to total sample
#'       \item \strong{ATC}: Control units get weight 1; treated units get
#'         weight \code{n_control_in_stratum / n_treated_in_stratum}
#'     }
#' }
#'
#' @examples
#' set.seed(42)
#' n <- 200
#' data <- data.frame(
#'   id = 1:n,
#'   age = rnorm(n, 40, 10),
#'   income = rnorm(n, 50000, 15000)
#' )
#' data$treatment <- rbinom(n, 1, plogis(-2 + 0.05 * data$age))
#' result <- subclass_match(treatment ~ age + income, data, treatment = "treatment")
#' print(result)
#'
#' @export
subclass_match <- function(formula = NULL,
                           data = NULL,
                           treatment = NULL,
                           n_subclasses = 5L,
                           ps = NULL,
                           ps_model = NULL,
                           estimand = "ATT") {

  # --- Validation ---
  if (is.null(data)) {
    stop("data must be provided", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop(sprintf("data must be a data.frame, got %s", class(data)[1]),
         call. = FALSE)
  }
  if (is.null(treatment)) {
    stop("treatment column name must be specified", call. = FALSE)
  }
  if (!(treatment %in% names(data))) {
    stop(sprintf("treatment column '%s' not found in data", treatment),
         call. = FALSE)
  }

  trt <- data[[treatment]]
  if (is.logical(trt)) trt <- as.integer(trt)
  if (!all(trt %in% c(0L, 1L))) {
    stop("treatment column must be binary (0/1 or TRUE/FALSE)", call. = FALSE)
  }

  n_subclasses <- as.integer(n_subclasses)
  if (is.na(n_subclasses) || n_subclasses < 2) {
    stop("n_subclasses must be an integer >= 2", call. = FALSE)
  }

  estimand <- toupper(estimand)
  if (!estimand %in% c("ATT", "ATE", "ATC")) {
    stop("estimand must be one of 'ATT', 'ATE', 'ATC'", call. = FALSE)
  }

  if (is.null(ps) && is.null(ps_model) && is.null(formula)) {
    stop("One of formula, ps, or ps_model must be provided", call. = FALSE)
  }

  # --- Compute propensity scores ---
  if (is.null(ps)) {
    if (is.null(ps_model)) {
      ps_model <- stats::glm(formula, data = data, family = stats::binomial())
    }
    # Predict on `data` explicitly so a pre-fitted ps_model produces scores
    # aligned to these rows rather than the model's original training frame.
    ps <- stats::predict(ps_model, newdata = data, type = "response")
  }

  if (length(ps) != nrow(data)) {
    stop(sprintf("ps length (%d) must equal nrow(data) (%d)",
                 length(ps), nrow(data)), call. = FALSE)
  }

  # Clip extreme values
  ps <- pmax(ps, 1e-8)
  ps <- pmin(ps, 1 - 1e-8)

  # --- Detect ID column ---
  id_col <- if ("id" %in% names(data)) "id" else NULL
  ids <- if (!is.null(id_col)) as.character(data[[id_col]]) else as.character(seq_len(nrow(data)))

  # --- Create subclasses based on PS quantiles ---
  # Use quantiles of the full PS distribution
  probs <- seq(0, 1, length.out = n_subclasses + 1)
  breaks <- stats::quantile(ps, probs = probs)
  # Ensure unique breaks (collapse if ties at quantile boundaries)
  breaks <- unique(breaks)
  # Adjust to create at least 2 strata
  if (length(breaks) < 3) {
    breaks <- c(min(ps) - 1e-10, stats::median(ps), max(ps) + 1e-10)
  }
  # Small adjustment to include boundary values
  breaks[1] <- breaks[1] - 1e-10
  breaks[length(breaks)] <- breaks[length(breaks)] + 1e-10

  subclass <- as.integer(cut(ps, breaks = breaks, include.lowest = TRUE))
  actual_n_subclasses <- max(subclass, na.rm = TRUE)

  # --- Compute weights per subclass ---
  treated_idx <- which(trt == 1L)
  control_idx <- which(trt == 0L)
  n_treated_total <- length(treated_idx)
  n_control_total <- length(control_idx)
  n_total <- nrow(data)

  weight <- rep(0, nrow(data))
  subclass_stats <- list()

  for (k in seq_len(actual_n_subclasses)) {
    in_k <- which(subclass == k)
    treated_in_k <- intersect(in_k, treated_idx)
    control_in_k <- intersect(in_k, control_idx)
    n_t <- length(treated_in_k)
    n_c <- length(control_in_k)
    has_overlap <- n_t > 0 && n_c > 0

    if (has_overlap) {
      if (estimand == "ATT") {
        weight[treated_in_k] <- 1
        weight[control_in_k] <- n_t / n_c
      } else if (estimand == "ATE") {
        # Each subclass contributes proportionally to its size (n_k / N). Within
        # the subclass a unit's weight is that stratum fraction spread over the
        # units of its treatment arm, so the arm's weights sum to n_k / N.
        stratum_frac <- length(in_k) / n_total
        weight[treated_in_k] <- stratum_frac / n_t
        weight[control_in_k] <- stratum_frac / n_c
      } else {
        # ATC
        weight[control_in_k] <- 1
        weight[treated_in_k] <- n_c / n_t
      }
    }

    subclass_stats[[k]] <- data.frame(
      subclass = k,
      n_treated = n_t,
      n_control = n_c,
      mean_ps = mean(ps[in_k]),
      ps_range_low = min(ps[in_k]),
      ps_range_high = max(ps[in_k]),
      has_overlap = has_overlap,
      stringsAsFactors = FALSE
    )
  }

  side <- ifelse(trt == 1L, "left", "right")

  matched_tbl <- tibble::tibble(
    id = ids,
    side = side,
    subclass = subclass,
    ps = ps,
    weight = weight
  )

  subclass_summary <- tibble::as_tibble(dplyr::bind_rows(subclass_stats))

  n_empty <- sum(!subclass_summary$has_overlap)

  info <- list(
    n_subclasses = actual_n_subclasses,
    n_empty_subclasses = n_empty,
    estimand = estimand,
    focal = "left",
    focal_discarded = 0L,
    n_left = n_treated_total,
    n_right = n_control_total,
    method = "subclassification",
    vars = if (!is.null(formula)) all.vars(formula)[-1] else NULL
  )

  result <- list(
    matched = matched_tbl,
    subclass_summary = subclass_summary,
    info = info
  )

  structure(result, class = c("subclass_result", "couplr_result"))
}


#' Print Method for Subclassification Results
#'
#' @param x A subclass_result object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.subclass_result <- function(x, ...) {
  cat("\nSubclassification Result\n")
  cat("========================\n\n")
  cat(sprintf("  Estimand: %s\n", x$info$estimand))
  cat(sprintf("  Subclasses: %d", x$info$n_subclasses))
  if (x$info$n_empty_subclasses > 0) {
    cat(sprintf(" (%d without overlap)", x$info$n_empty_subclasses))
  }
  cat("\n")
  cat(sprintf("  Treated (left):  %d\n", x$info$n_left))
  cat(sprintf("  Control (right): %d\n", x$info$n_right))
  cat("\n  Subclass summary:\n")
  print(x$subclass_summary, n = Inf)
  cat("\n")
  invisible(x)
}
