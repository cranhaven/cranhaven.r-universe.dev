#' Prepare Model Output for OES Plots
#'
#' Prepare output of linear modeling object into a tidy data table to feed
#' into OES plotting function
#'
#' \code{oes_prep()} takes a linear modeling output object (from \code{lm()}
#' or \code{lm_robust()}) and returns a tidy tibble of estimates,
#' confidence bounds, and related quantities ready for oes_plot to plot.
#' Functionality for \code{lm_lin()} objects is in development.
#'
#' @param model An object of class \code{lm} or \code{lm_robust}
#'
#' @param treatment_vars An optional character vector of treatment arm names. One
#' of \code{treatment_vars} or \code{treatment_arms} must be supplied.
#'
#' @param treatment_arms An optional numeric value indicating the number of treatment
#' arms. (Not required if treatment_vars is given explicitly.) One
#' of \code{treatment_vars} or \code{treatment_arms} must be supplied.
#'
#' @param scale String indicating the \eqn{y}-axis scale. Default is \code{'response'}.
#' For binary outcomes, it may be desirable to set to \code{"percentage"}.
#'
#' @param treatment_labels Optional vector of string labels providing treatment condition(s)
#'
#' @param control_label Optional string providing control condition label
#'
#' @param alpha_level The level at which to reject the null hypothesis for adding
#' asterisks to plots. Set to 0.05 by default. This value also determines the size
#' of the confidence intervals (\code{alpha_level = 0.05} corresponds to 95% confidence
#' intervals).
#'
#' @return A tibble of \eqn{T+1} rows and 8 columns, where \eqn{T} is the number
#' of treatment conditions specified via \code{treatment_vars} or
#' \code{treatment_arms}.
#'
#' @author Miles Williams
#'
#' @importFrom estimatr lm_robust
#'
#' @importFrom rlang .data
#'
#' @importFrom stats coef formula lm model.frame model.matrix reorder
#' @importFrom stats sd vcov qt df.residual
#'
#' @examples
#'
#' data(df_oes)
#'
#' # Single binary treatment:
#' fit <- lm(y1 ~ x1, df_oes)
#'
#' # Multiple treatment conditions:
#' fit2 <- lm(y2 ~ x2, df_oes)
#'
#' # Using HC2 SE's from lm_robust():
#' fit_robust <- estimatr::lm_robust(y1 ~ x1, df_oes)
#' fit_robust2 <- estimatr::lm_robust(y2 ~ x2, df_oes)
#'
#' # Using covariates and lm():
#' fit_covars <- lm(y2 ~ x2 + z1 + z2 + z3, df_oes)
#'
#' # Using covariates and lm_robust():
#' fit_covars_robust <- estimatr::lm_robust(y2 ~ x2 + z1 + z2 + z3, df_oes)
#'
#' # Example specifying number of treatment arms:
#' oes_prep(fit, treatment_arms = 1)
#'
#' # Example specifying name of treatment variable:
#' oes_prep(fit, treatment_vars = "x1")
#'
#' # Example reporting outcomes as percentages:
#' oes_prep(fit, treatment_vars = "x1", scale = "percentage")
#'
#' # Example specifying several treatment arms, labels, etc.:
#' oes_prep(fit2, treatment_arms = 3,
#'   treatment_labels = c(
#'     "Email",
#'     "Email +\nReward",
#'     "Email +\nRisk"),
#'   control_label = "Status Quo",
#'   scale = "percentage")
#'
#' # Examples with lm_robust():
#' oes_prep(fit_robust, treatment_arms = 1)
#' oes_prep(fit_robust2, treatment_arms = 3)
#'
#' # Examples with covariates:
#' oes_prep(fit_covars, treatment_arms = 3)
#' oes_prep(fit_covars_robust, treatment_arms = 3)
#'
#' @export

oes_prep <- function(model,
                     treatment_vars = NULL,
                     treatment_arms = NULL,
                     scale = c('response', 'percentage'),
                     treatment_labels,
                     control_label,
                     alpha_level = 0.05
) {

  # Stop and return error message if dependencies are not installed
  if(!requireNamespace('sandwich', quietly = TRUE) & isa(model, 'lm')) {
    stop("Package 'sandwich' required for this function to work. Please install it.",
         call. = FALSE)
  }

  if(!requireNamespace('estimatr', quietly = TRUE)) {
    stop("Package 'estimatr' required for this function to work. Please install it.",
         call. = FALSE)
  }

  if(!requireNamespace('dplyr', quietly = TRUE)) {
    stop("Package 'dplyr' required for this function to work. Please install it.",
         call. = FALSE)
  }

  if(!requireNamespace('tibble', quietly = TRUE)) {
    stop("Package 'tibble' required for this function to work. Please install it.",
         call. = FALSE)
  }

  # Stop and return error message if both treatment_vars
  # and treatment_arms are NULL

  if(all(is.null(c(treatment_vars, treatment_arms)))) {
    stop("Specify either the number of treatment arms or the treatment variable names explicitly.",
         call. = FALSE)
  }

  # Store control and response values

  # Outcome:
  response <- model.frame(model)[, 1]

  # Model matrix:
  if(isa(model, 'lm')) {
    m.mat <- model.matrix(model)
  } else {
    m.mat <- model.matrix(
      lm(
        formula(model),
        data = model.frame(model)
      )
    )
  }

  # Treatment variable names:
  if(is.null(treatment_vars)){
    treatment_vars <- colnames(m.mat[, c(1, 1 + 1:treatment_arms)])[-1]
  }

  # Observed control group mean:
  if(length(treatment_vars) == 1) {
    control_mean <- mean(response[which(m.mat[, treatment_vars] == 0)])
  } else {
    control_mean <- mean(response[which(rowSums(m.mat[, treatment_vars] != 0) == 0)])
  }

  # Standard error for the control:
  if(length(treatment_vars) == 1) {
    control_se <- sd(response[which(m.mat[, treatment_vars] == 0)]) /
      sqrt(length(response[which(m.mat[, treatment_vars] == 0)]))
  } else {
    control_se <- sd(response[which(rowSums(m.mat[, treatment_vars] != 0) == 0)]) /
      sqrt(length(response[which(rowSums(m.mat[, treatment_vars] != 0) == 0)]))
  }

  # Predicted means of response under treatment arms:
  preds <- c(
    control_mean + coef(model)[treatment_vars]
  )

  # Standard errors for treatment conditions:
  if(isa(model, 'lm')) {
    robcov <- sandwich::vcovHC(model, 'HC2')
  } else {
    robcov <- vcov(model)
  }

  ses <- sqrt(diag(robcov))[treatment_vars]

  # If Binary response results are to be reported in percentage points:
  if(scale[1] == 'percentage') {
    control_mean <- 100 * control_mean
    control_se <- 100 * control_se
    preds <- 100 * preds
    ses <- 100 * ses
  }

  # Generate tidy data frame for plotting response values:

  # 1. control and treatment labels
  if(missing(treatment_labels)) {
    if(length(treatment_vars) == 1) {
      treatment_labels <- "Treatment"
    } else {
      treatment_labels <- paste("Treatment", 1:length(treatment_vars))
    }
  } else {
    treatment_labels <- treatment_labels
  }
  if(missing(control_label)) {
    control_label <- "Control"
  } else {
    control_label <- control_label
  }

  term_labels <- c(control_label, treatment_labels)

  # 2. control and treatment values
  estimate <- c(control_mean, preds)

  # 3. control and treatment confidence intervals
  crit_value <- qt(1 - alpha_level / 2, df = df.residual(model))
  lo.ci <- estimate - crit_value * c(control_se, ses)
  hi.ci <- estimate + crit_value * c(control_se, ses)

  # Extract p-values:
  p_values <- broom::tidy(model) |>
    dplyr::filter(.data$term %in% treatment_vars) |>
    dplyr::select(.data$p.value) |>
    unlist() |>
    unname()

  # Store in table:
  plot_data <- tibble::tibble(
    term = term_labels,
    estimate = estimate,
    lo.ci = lo.ci,
    hi.ci = hi.ci,
    p.value = c(1, p_values),
    alpha.level = alpha_level,
    ord = 1:(1 + length(treatment_vars)),
    response = c('control', rep('treatment', len = length(treatment_vars)))
  )

  # Return the tidy data table:
  return(plot_data)
}
