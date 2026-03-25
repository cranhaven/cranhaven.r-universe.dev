#' DID-IV decomposition
#' 
#' twfeiv_decomp() is a function that decomposes the TWFEIV estimator into all possible Wald-DID estimators.
#' 
#' @param formula A formula object of the form `Y ~ D + controls | controls + Z`, where:
#'   - Y is the outcome variable,
#'   - D is the treatment variable,
#'   - Z is a binary instrumental variable, and
#'   - controls are optional control variables.
#' Do not include fixed effects (e.g., individual or time dummies) in the control variables.
#' @param data A data frame containing all variables used in the formula, as well as the variables specified by id_var and time_var.
#' @param id_var The name of id variable.
#' @param time_var The name of time variable.
#' @param summary_output Logical. If TRUE, prints a summary table showing, for each design type, the total weight and the weighted average of the Wald-DID estimates. If FALSE (the default), no summary is printed.
#' 
#' @return
#' If no control variables are included in the formula, the function returns a data frame named exposed_unexposed_combinations 
#' which contains the Wald-DID estimates and corresponding weights for each exposed/unexposed cohort pair.
#'
#' If control variables are included, the function returns a list named decomposition_list containing:
#' \describe{
#'   \item{within_IV_coefficient}{Numeric. The coefficient from the within-IV regression.}
#'   \item{between_IV_coefficient}{Numeric. The coefficient from the between-IV regression.}
#'   \item{Omega}{Numeric. The weight on the within-IV coefficient in the TWFEIV estimator, such that \eqn{TWFEIV = \Omega \times \text{within} + (1 - \Omega) \times \text{between}}.}
#'   \item{exposed_unexposed_combinations}{A data.frame with the between-IV coefficients and corresponding weights for each exposed/unexposed cohort pair.}
#' } 
#' @examples
#' # Load example dataset
#' data(simulation_data)
#' head(simulation_data)
#' 
#' # Example without controls
#' decomposition_result_without_controls <- twfeiv_decomp(outcome ~ treatment | instrument,
#'                                       data = simulation_data,
#'                                       id_var = "id",
#'                                       time_var = "time")
#' 
#' # Example with controls
#' decomposition_result_with_controls <- twfeiv_decomp(
#'   outcome ~ treatment + control1 + control2 |control1 + control2 + instrument,
#'   data = simulation_data,
#'   id_var = "id",
#'   time_var = "time"
#' )
#' 
#' @import dplyr
#' @import Formula
#' @importFrom stats aggregate as.formula coef cov lm model.matrix predict reformulate residuals var weighted.mean
#' @importFrom magrittr %>%
#' @importFrom AER ivreg
#' 
#' @export
twfeiv_decomp <- function(formula,
                          data,
                          id_var,
                          time_var,
                          summary_output = FALSE){
# Extract variable names from formula
var_names <- extract_variable_names(formula)
outcome_var <- var_names$outcome_var
treatment_var <- var_names$treatment_var
instrument_var <- var_names$instrument_var
control_vars <- var_names$control_vars

# Rename variables in the data
data <- rename_variables(data, id_var, time_var, outcome_var, treatment_var, instrument_var)

# Check whether the data are a balanced panel
obs_per_id <- aggregate(time ~ id, data = data, FUN = length)
if (length(unique(obs_per_id$time)) != 1) {
    stop("The dataset is an unbalanced panel")
  }

# Check whether the data contain NA observations
na_check <- sum(is.na(data[, c("id", "time", "outcome", "treatment", "instrument")]))
if (length(control_vars) > 0) {
  control_formula <- reformulate(
    control_vars,
    response = NULL,
    intercept = FALSE  
  )
  control_matrix <- model.matrix(control_formula, data = data)
  na_check_control <- 1 - (nrow(control_matrix) == nrow(data))
  na_check <- na_check + na_check_control
}
if (na_check > 0) {
  stop("NA observations are detected in the data")
}

# Create cohort variable in the data
data <- create_cohort(data = data)

# Drop already exposed units in the data
data <- data %>% 
    filter(cohort != min(time))

# Check whether the instrument assignment is staggered
data <- data %>%
    mutate(
      staggered_indicator = case_when(
        cohort == 999999 ~ 1,
        time >= cohort & instrument == 1 ~ 1,
        time < cohort & instrument == 0 ~ 1,
        TRUE ~ 0
      )
    )

if (!all(data$staggered_indicator == 1)) {
  stop("The instrument assignment is non-staggered")
  }

# Create cohort share variable in the data
data <- create_cohort_share(data = data)

# Create time share variable in the data
data <- create_time_share(data = data)

# Create exposed/unexposed combinations
exposed_unexposed_combinations <- create_exposed_unexposed_combinations(data = data, control_vars = control_vars)

if (length(control_vars) == 0) {
for (i in seq_len(nrow(exposed_unexposed_combinations))){
  exposed_cohort <- exposed_unexposed_combinations$exposed_cohort[i]
  unexposed_cohort <- exposed_unexposed_combinations$unexposed_cohort[i]

  # Subset the data
  data_subset <- subset_data(data = data, exposed_cohort = exposed_cohort, unexposed_cohort = unexposed_cohort)

  # Calculate the Wald-DID estimate by running the IV regressin within data_subset
  Wald_DID_estimate <- ivreg(outcome ~ treatment + factor(time) + factor(id) | factor(time) + factor(id) + instrument, data = data_subset)$coef["treatment"]
  exposed_unexposed_combinations[i, "Wald_DID_estimate"] <- Wald_DID_estimate

  # Calculate the numerator of the weight
  weight_numerator <- calculate_weight_numerator(data = data, data_subset = data_subset, exposed_cohort = exposed_cohort, unexposed_cohort = unexposed_cohort)
  exposed_unexposed_combinations[i, "weight_numerator"] <- weight_numerator
}

# Calculate the denominator of the weight
exposed_unexposed_combinations[, "weight_denominator"] <- sum(exposed_unexposed_combinations$weight_numerator)

# Calculate the weight
exposed_unexposed_combinations <- exposed_unexposed_combinations %>%
  mutate(weight = weight_numerator / weight_denominator)

# drop weight_numerator and weight_denominator in exposed_unexposed_combinations
exposed_unexposed_combinations <- exposed_unexposed_combinations %>%
  select(-weight_numerator, -weight_denominator)

# Print summary
if (summary_output == TRUE){
  print_summary(data = exposed_unexposed_combinations, return_df = TRUE)
}

return(exposed_unexposed_combinations)

} else {
  # Calculate the residuals from regressing Z on time and individual fixed effects
  data[["residual_Z"]] <- residuals(lm(instrument ~ factor(id) + factor(time), data = data))
  
  # Calculate the residuals from regressing each control on time and individual fixed effects
  data <- calculate_residuals(data, control_vars)

  # Calculate various means of the individual linear projection
  data <- calculate_mean_linear_projection(data, control_vars)

  # Calculate the residuals \tilde{z_{i,t}} from regressing instrument on controls and two way fixed effects
  data[["z_it"]] <- residuals(lm(reformulate(c("factor(id)", "factor(time)", control_vars),
                          response = "instrument"), data = data))

  # Calculate the within and between terms of \tilde{z_{i,t}}
  data <- calculate_within_between_terms(data = data)

  # Calculate the within IV coefficient
  # Cope with NA case (when within_z is zero vector)
  within_IV_coefficient <- cov(data$outcome, data$within_z)/cov(data$treatment, data$within_z)
  within_IV_coefficient <- ifelse(is.na(within_IV_coefficient), 0, within_IV_coefficient)

  # Calculate the between IV coefficient
  # Cope with NA case (when within_z is zero vector)
  between_IV_coefficient <- cov(data$outcome, data$between_z)/cov(data$treatment, data$between_z)
  between_IV_coefficient <- ifelse(is.na(between_IV_coefficient), 0, between_IV_coefficient)

  # Calculate Omega
  cov_within <- cov(data$treatment, data$within_z)
  cov_between <- cov(data$treatment, data$between_z)
  Omega <- cov_within/(cov_within + cov_between)
  
  # Decompose the between term
  for (i in seq_len(nrow(exposed_unexposed_combinations))){
  exposed_cohort <- exposed_unexposed_combinations$exposed_cohort[i]
  unexposed_cohort <- exposed_unexposed_combinations$unexposed_cohort[i]

  # Subset the data
  data_subset <- data %>%
  filter(cohort %in% c(exposed_cohort, unexposed_cohort))
  
  # Calculate the (k,l) between IV coefficient
  calculate_weight_and_between_IV_coefficient_kl <- calculate_weight_and_between_IV_coefficient_kl(data_subset, control_vars)
  between_IV_coefficient_kl <- calculate_weight_and_between_IV_coefficient_kl$between_IV_coefficient_kl
  weight_kl <- calculate_weight_and_between_IV_coefficient_kl$weight_kl
   
  exposed_unexposed_combinations[i, "between_IV_coefficient_kl"] <- between_IV_coefficient_kl
  exposed_unexposed_combinations[i, "weight_kl"] <- weight_kl
}

  # Normalization
  total_weight <- sum(exposed_unexposed_combinations$weight_kl, na.rm = TRUE)
  exposed_unexposed_combinations$weight_kl <- exposed_unexposed_combinations$weight_kl / total_weight

  # Print summary
  if (summary_output == TRUE){
  print_summary(data = exposed_unexposed_combinations, return_df = TRUE)
  }

  decomposition_list <- list("within_IV_coefficient" = within_IV_coefficient,
                             "between_IV_coefficient" = between_IV_coefficient,
                             "Omega" = Omega,
                             "exposed_unexposed_combinations" = exposed_unexposed_combinations)

  return(decomposition_list)
}
                          }

#' Extract variable names from a formula used in twfeiv_decomp.
#' 
#' This helper function parses a two-part formula used in twfeiv_decomp() and returns the names of the outcome, treatment, instrument, and control variables.
#' 
#' @param formula A formula object of the form Y ~ D + controls | controls + Z.
#' 
#' @return A named list with the following elements:
#' - outcome_var: The name of the outcome variable on the left-hand side.
#' - treatment_var: The name of the treatment variable (appears only on the first right-hand side).
#' - instrument_var: The name of the instrumental variable (appears only on the second right-hand side).
#' - control_vars: The names of the variables common to both right-hand sides, treated as control variables.
#' 
#' @noRd 
extract_variable_names <- function(formula) {
  formula <- Formula::as.Formula(formula)

  outcome_var <- all.vars(formula(formula, lhs = 1, rhs = 0))
  rhs_vars_1 <- all.vars(formula(formula, lhs = 0, rhs = 1))
  rhs_vars_2 <- all.vars(formula(formula, lhs = 0, rhs = 2))

  control_vars <- intersect(rhs_vars_1, rhs_vars_2)
  treatment_var <- setdiff(rhs_vars_1, control_vars)
  instrument_var <- setdiff(rhs_vars_2, control_vars)

  return(list(
    outcome_var = outcome_var,
    treatment_var = treatment_var,
    instrument_var = instrument_var,
    control_vars = control_vars
  ))
}

#' Rename key variables in the data for internal use.
#' 
#' This helper function renames the id, time, outcome, treatment, and instrument variables in the input data to standardized names ("id", "time", "outcome", "treatment", "instrument").
#' This ensures consistent column naming within the package.
#' 
#' @param data A data frame containing the variables to be renamed.
#' @param id_var The name of the id variable.
#' @param time_var The name of the time variable.
#' @param outcome_var The name of the outcome variable.
#' @param treatment_var The name of the treatment variable.
#' @param instrument_var The name of the instrumental variable.
#' 
#' @return A data.frame with renamed columns.
#' 
#' @noRd
rename_variables <- function(data, id_var, time_var, outcome_var, treatment_var, instrument_var) {
    colnames(data)[colnames(data) == id_var] <- "id"
    colnames(data)[colnames(data) == time_var] <- "time"
    colnames(data)[colnames(data) == outcome_var] <- "outcome"
    colnames(data)[colnames(data) == treatment_var] <- "treatment"
    colnames(data)[colnames(data) == instrument_var] <- "instrument"
    return(data)
}

#' Create cohort variable in the data based on the intial exposure to the instrument.
#' 
#' This function creates a new variable "cohort", defined as the first time period (min time) in which the instrumental variable equals 1 for each unit (id).
#' Units that are never exposed to the instrument receive a cohort value of 999999.
#' 
#' @param data A data.frame.
#' 
#' @return A data.frame with the new variable "cohort".
#' 
#' @noRd
create_cohort <- function(data) { 
  id_cohort_combinations <- data %>%
    filter(instrument == 1) %>%
    group_by(id) %>%
    summarise(cohort = min(time), .groups = "drop")

  data <- data %>%
    left_join(id_cohort_combinations, by = "id") %>%
    mutate(cohort = if_else(is.na(cohort), 999999, cohort))
  return(data)
}

#' Create cohort share variable in the data.
#' 
#' This function calculates the proportion of observations in each cohort (cohort share)
#' and adds it to the input data frame as a new variable called "cohort_share".
#' The share is defined as the number of observations in each cohort divided by the total number of observations.
#' 
#' @param data A data.frame.
#' 
#' @return A data.frame with the new variable "cohort_share".
#' 
#' @noRd
create_cohort_share <- function(data) { 
  data_cohort_share <- data %>%
    group_by(cohort) %>%
    summarise(count = n()) %>%
    mutate(cohort_share = count / sum(count)) %>%
    select(cohort, cohort_share)
  data <- data %>%
    left_join(data_cohort_share, by = "cohort")
  
  return(data)
}

#' Create time share variable in the data.
#' 
#' This function computes the average exposure to the instrument over time
#' for each cohort. The resulting variable, "time_share", reflects the average proportion of
#' periods in which units in a given cohort are exposed to the instrument.
#' For never-treated units (cohort == 999999), the time share is set to 0 by definition.
#'  
#' @param data A data.frame.
#' 
#' @return A data.frame with the new variable "time_share".
#' 
#' @noRd
create_time_share <- function(data) { 
  data_time_share <- data %>%
    dplyr::group_by(cohort) %>%
    dplyr::summarise(time_share = mean(instrument), .groups = "drop")

  data_time_share$time_share[data_time_share$cohort == 999999] <- 0
  
  data <- data %>%
    left_join(data_time_share, by = "cohort")

  return(data = data)
}

#' Create combinations of exposed and unexposed cohorts.
#'
#' This function generates all valid pairs of (exposed_cohort, unexposed_cohort) to be used in DID-IV decomposition.
#' If no control variables are specified, all possible pairs with different cohorts are allowed.
#' If control variables are included, only pairs where the unexposed cohort occurs after the exposed cohort are considered valid.
#' 
#' @param data A data.frame.
#' 
#' @return A data.frame with columns 'exposed_cohort' and 'unexposed_cohort'.
#' 
#' @noRd
create_exposed_unexposed_combinations <- function(data, control_vars) {
  if (length(control_vars) == 0) {
  all_cohorts <- unique(data$cohort)

  exposed_cohort_candidates <- all_cohorts[all_cohorts != 999999]
  unexposed_cohort_candidates <- all_cohorts

  exposed_unexposed_combinations <- expand.grid(
    exposed_cohort = exposed_cohort_candidates,
    unexposed_cohort = unexposed_cohort_candidates
  ) %>%
  filter(exposed_cohort != unexposed_cohort)

  exposed_unexposed_combinations <- exposed_unexposed_combinations %>%
  mutate(design_type = case_when(
    unexposed_cohort == 999999 ~ "Exposed vs Unexposed",
    unexposed_cohort > exposed_cohort ~ "Exposed vs Not Yet Exposed",
    TRUE ~ "Exposed vs Exposed Shift"
  ))

  return(exposed_unexposed_combinations)
  } else {
  all_cohorts <- unique(data$cohort)

  exposed_cohort_candidates <- all_cohorts[all_cohorts != 999999]
  unexposed_cohort_candidates <- all_cohorts

  exposed_unexposed_combinations <- expand.grid(
    exposed_cohort = exposed_cohort_candidates,
    unexposed_cohort = unexposed_cohort_candidates
  ) %>%
  filter(unexposed_cohort > exposed_cohort)

  exposed_unexposed_combinations <- exposed_unexposed_combinations %>%
  mutate(design_type = case_when(
    unexposed_cohort == 999999 ~ "Exposed vs Unexposed",
    TRUE ~ "Early Exposed vs Later Exposed"
  ))
  
  return(exposed_unexposed_combinations)
  }
}

#' Subset the data for a given pair of cohorts.
#' 
#' This function extracts observations belonging to the specified exposed and unexposed cohorts.
#' It then filters the data to retain only the relevant time periods for comparison, depending on the relative timing of the cohorts:
#' - If the exposed cohort occurs after the unexposed cohort, the function keeps observations where time is greater than or equal to the unexposed cohort.
#' - If the exposed cohort occurs before the unexposed cohort, the function keeps observations where time is less than the unexposed cohort.
#' 
#' @param data A data.frame. 
#' @param exposed_cohort The value of the exposed cohort from the cohort pair.
#' @param unexposed_cohort The value of the unexposed cohort from the cohort pair.
#' 
#' @return A data.frame that contains only the two cohorts and relevant periods.
#' 
#' @noRd
subset_data <- function(data, exposed_cohort, unexposed_cohort){
  data_subset <- data %>%
  filter(cohort %in% c(exposed_cohort, unexposed_cohort)) 

  if (exposed_cohort > unexposed_cohort) {
    data_subset <- data_subset %>%
      filter(time >= unexposed_cohort)
  } else {
    data_subset <- data_subset %>%
      filter(time < unexposed_cohort)
  }

  return(data_subset)
}

#' Calculate the numerator of the weight.
#' 
#' @param data A data.frame. 
#' @param data_subset A subset of the data.frame used for estimating the DID estimate in the treatment.
#' @param exposed_cohort The value of the exposed cohort from the cohort pair.
#' @param unexposed_cohort The value of the unexposed cohort from the cohort pair.
#' 
#' @return A numeric scalar representing the numerator of the weight for the given cohort pair.
#' 
#' @noRd
calculate_weight_numerator <- function(data, data_subset, exposed_cohort, unexposed_cohort) {
  # TWFE regression in the subsample (which yields the DID estimate in the treatment)
  DID_estimate_treatment <- unname(lm(treatment ~ instrument + factor(time) + factor(id), data = data_subset)$coef["instrument"])

  if (unexposed_cohort == 999999) {
    n_u <- unique(data$cohort_share[data$cohort == unexposed_cohort])
    n_k <- unique(data$cohort_share[data$cohort == exposed_cohort])
    n_ku <- n_k / (n_u + n_k)
    Z_k <- unique(data$time_share[data$cohort == exposed_cohort])
    sample_share_squared <- (n_k + n_u)^2
    variance <- n_ku * (1 - n_ku) * Z_k * (1 - Z_k)
    first_stage_weight <- sample_share_squared * variance
    weight_numerator <- first_stage_weight * DID_estimate_treatment
  } else if (exposed_cohort < unexposed_cohort) {
    n_k <- unique(data$cohort_share[data$cohort == exposed_cohort])
    n_l <- unique(data$cohort_share[data$cohort == unexposed_cohort])
    n_kl <- n_k / (n_k + n_l)
    Z_k <- unique(data$time_share[data$cohort == exposed_cohort])
    Z_l <- unique(data$time_share[data$cohort == unexposed_cohort])
    sample_share_squared <- ((n_k + n_l) * (1 - Z_l))^2
    variance <- n_kl * (1 - n_kl) * ((Z_k - Z_l) / (1 - Z_l)) * ((1 - Z_k) / (1 - Z_l))
    first_stage_weight <- sample_share_squared * variance
    weight_numerator <- first_stage_weight * DID_estimate_treatment
  } else {
    n_k <- unique(data$cohort_share[data$cohort == unexposed_cohort])
    n_l <- unique(data$cohort_share[data$cohort == exposed_cohort])
    n_kl <- n_k / (n_k + n_l)
    Z_k <- unique(data$time_share[data$cohort == unexposed_cohort])
    Z_l <- unique(data$time_share[data$cohort == exposed_cohort])
    sample_share_squared <- ((n_k + n_l) * Z_k)^2
    variance <- n_kl * (1 - n_kl) * (Z_l / Z_k) * ((Z_k - Z_l) / Z_k)
    first_stage_weight <- sample_share_squared * variance
    weight_numerator <- first_stage_weight * DID_estimate_treatment
  }
  return(weight_numerator)
}

#' Print the summary.
#' 
#' @param data A data.frame. 
#' @param return_df Logical. If TRUE, returns the summary data.frame.
#' 
#' @return Invisibly prints the summary to console. Returns a data.frame if return_df = TRUE.
#' 
#' @export
print_summary <- function(data, return_df = FALSE) {
  if ("Wald_DID_estimate" %in% names(data)) {
    # no controls
    summary_data <- data %>%
      group_by(design_type) %>%
      summarise(
        weight_sum = round(sum(weight), 5),
        Weighted_average_Wald_DID = round(weighted.mean(Wald_DID_estimate, weight), 5),
        .groups = "drop"
      )
  } else if ("between_IV_coefficient_kl" %in% names(data)) {
    # with controls
    summary_data <- data %>%
      group_by(design_type) %>%
      summarise(
        weight_sum = round(sum(weight_kl), 5),
        Weighted_average_between_IV_coefficient_kl = round(weighted.mean(between_IV_coefficient_kl, weight_kl), 5),
        .groups = "drop"
      )
  } else {
    stop("Neither Wald_DID_estimate nor between_IV_coefficient found in data.")
  }

  print(summary_data)

  if (return_df) {
    return(summary_data)
  }
}

#' Calculate the residuals.
#' 
#' @param data A data.frame. 
#' @param control_vars A character vector of control variable names.
#' 
#' @return A data.frame with new residual variables.
#' 
#' @noRd
calculate_residuals <- function(data, control_vars) {
  for (var in control_vars) {
    formula <- paste0(var, " ~ factor(id) + factor(time)")
    residual_name <- paste0("residual_", var)
    data[[residual_name]] <- residuals(lm(as.formula(formula), data = data))
  }
  return(data)
}

#' Calculate various means of the individual linear projection.
#' 
#' @param data A data.frame. 
#' @param control_vars
#' 
#' @return A data.frame with new mean variables.
#' 
#' @noRd
calculate_mean_linear_projection <- function(data, control_vars) {
  # Obtain the regression coefficient, \hat{Gamma}
  residual_controls <- paste0("residual_", control_vars)
  formula <- paste("residual_Z ~", paste(residual_controls, collapse = " + "), " - 1")
  coefficients <- coef(lm(as.formula(formula), data = data))
 
  # Calculate \hat{Gamma} \times X_{i,t}
  projection_id_time <- rep(0, nrow(data))
  for (i in seq_along(control_vars)) {
  projection_id_time <- projection_id_time + coefficients[i] * data[[control_vars[i]]]
  }

  data$projection_id_time <- projection_id_time
  
  # Calculate \hat{Gamma} \times \bar{X_{i}}
  # Calculate \bar{X_{i}}
  for (var in control_vars) {
    control_id_average <- paste0(var, "_id_average")
    data <- data %>%
    group_by(id) %>%
    mutate(!!control_id_average := mean(.data[[var]], na.rm = TRUE)) %>%
    ungroup()
  }
  id_average_vars <- paste0(control_vars, "_id_average")
  projection_id <- rep(0, nrow(data))

  for (i in seq_along(control_vars)) {
    projection_id <- projection_id + coefficients[i] * data[[id_average_vars[i]]]
  }

  data$projection_id <- projection_id
  
  # Calculate \hat{Gamma} \times \bar{X_{t}}
  # Calculate \bar{X_{t}}
  for (var in control_vars) {
    control_time_average <- paste0(var, "_time_average")
    data <- data %>%
    group_by(time) %>%
    mutate(!!control_time_average := mean(.data[[var]], na.rm = TRUE)) %>%
    ungroup()
  }
  time_average_vars <- paste0(control_vars, "_time_average")
  projection_time <- rep(0, nrow(data))

  for (i in seq_along(control_vars)) {
    projection_time <- projection_time + coefficients[i] * data[[time_average_vars[i]]]
  }

  data$projection_time <- projection_time
  
  # Calculate \hat{Gamma} \times \bar{X_{k,t}}
  # Calculate \bar{X_{k,t}}
  for (var in control_vars) {
    control_cohort_time_average <- paste0(var, "_cohort_time_average")
    data <- data %>%
    group_by(cohort, time) %>%
    mutate(!!control_cohort_time_average := mean(.data[[var]], na.rm = TRUE)) %>%
    ungroup()
  }
  cohort_time_average_vars <- paste0(control_vars, "_cohort_time_average")
  projection_cohort_time <- rep(0, nrow(data))

  for (i in seq_along(control_vars)) {
    projection_cohort_time <- projection_cohort_time + coefficients[i] * data[[cohort_time_average_vars[i]]]
  }

  data$projection_cohort_time <- projection_cohort_time

  # Calculate \hat{Gamma} \times \bar{X_{k}}
  # Calculate \bar{X_{k}}
  for (var in control_vars) {
    control_cohort_average <- paste0(var, "_cohort_average")
    data <- data %>%
    group_by(cohort) %>%
    mutate(!!control_cohort_average := mean(.data[[var]], na.rm = TRUE)) %>%
    ungroup()
  }
  cohort_average_vars <- paste0(control_vars, "_cohort_average")
  projection_cohort <- rep(0, nrow(data))

  for (i in seq_along(control_vars)) {
    projection_cohort <- projection_cohort + coefficients[i] * data[[cohort_average_vars[i]]]
  }

  data$projection_cohort <- projection_cohort
  
  # Calculate \hat{Gamma} \times \bar{\bar{X}}
  # Calculate \bar{\bar{X}}
  for (var in control_vars) {
    control_grand_mean <- paste0(var, "_grand_mean")
    data <- data %>%
    mutate(!!control_grand_mean := mean(.data[[var]], na.rm = TRUE)) %>%
    ungroup()
  }
  grand_mean_vars <- paste0(control_vars, "_grand_mean")
  projection_grand_mean <- rep(0, nrow(data))

  for (i in seq_along(control_vars)) {
    projection_grand_mean <- projection_grand_mean + coefficients[i] * data[[grand_mean_vars[i]]]
  }

  data$projection_grand_mean <- projection_grand_mean

  return(data)
}

#' Calculate the within and between terms of \tilde{z_{i,t}}.
#' 
#' @param data A data.frame. 
#' 
#' @return A data.frame with within and between terms of \tilde{z_{i,t}}.
#' 
#' @noRd
calculate_within_between_terms <- function(data) {
  # Calculate \bar{z_{i}}
  data <- data %>%
  group_by(id) %>%
  mutate(z_i_bar = mean(z_it, na.rm = TRUE)) %>%
  ungroup()
  
  # Calculate \bar{z_{t}}
  data <- data %>%
  group_by(time) %>%
  mutate(z_t_bar = mean(z_it, na.rm = TRUE)) %>%
  ungroup()

  # Calculate \bar\bar{z}
  data <- data %>%
  mutate(z_bar_bar = mean(z_it, na.rm = TRUE)) %>%
  ungroup()
  
  # Calculate \bar{z_{k,t}}
  data <- data %>%
  group_by(cohort, time) %>%
  mutate(z_kt_bar = mean(z_it, na.rm = TRUE)) %>%
  ungroup()

  # Calculate \bar{z_k}
  data <- data %>%
  group_by(cohort) %>%
  mutate(z_k_bar = mean(z_it, na.rm = TRUE)) %>%
  ungroup()

  # Calculate the within term of \tilde{z_{i,t}}
  data <- data %>%
    mutate(within_z = (z_it - z_i_bar) - (z_kt_bar - z_k_bar))
  
  # Calculate the between term of \tilde{z_{i,t}}
  data <- data %>%
    mutate(between_z = ((z_kt_bar - z_k_bar) - (z_t_bar - z_bar_bar))) 
  
  return(data)
}

#' Partial out time and unit fixed effects from instrument.
#'
#' @param data A data.frame.
#'
#' @return A list with two elements:
#'   \item{data}{A data.frame containing the input data with an added column \code{instrument_residuals}, 
#'   the residuals from regressing the instrument on unit and time fixed effects.}
#'   \item{V_Z}{Numeric. The estimated variance of the residualized instrument.}
#'
#' @noRd
partial_out_instrument <- function(data) {
    data$instrument_residuals <- residuals(lm(instrument ~ factor(id) + factor(time), data = data))   
    N <- nrow(data)
    V_Z <- var(data$instrument_residuals) * (N - 1) / N
    r_list <- list(data = data, V_Z = V_Z) 
    return(r_list)
}

#' Partial out time and unit fixed effects from cohort-time average control variables.
#'
#' @param data A data.frame with cohort_time_average variables.
#' @param control_vars A character vector of original control variables.
#'
#' @return A data.frame with new residualized variables.
#'
#' @noRd
partial_out_cohort_time_covariate <- function(data, control_vars) {
  cohort_time_average_vars <- paste0(control_vars, "_cohort_time_average")
  
  for (var in cohort_time_average_vars) {
    formula_var <- as.formula(paste0(var, " ~ factor(id) + factor(time)"))
    data[[paste0(var, "_residuals")]] <- residuals(lm(formula_var, data = data))
  }  
  return(data)
}

#' Calculate p_tilde_kl (linear projection from instrument_residuals on cohort_time_average_residuals variables) and r_squared. 
#' 
#' @param data A data.frame. 
#' @param control_vars A character vector of original control variable names.
#'
#' @return A list with two elements:
#'   \item{data}{A data.frame containing the input data with an additional column \code{p_tilde_kl}, 
#'   the fitted values from the linear projection.}
#'   \item{r_squared}{Numeric. The R-squared statistic from the projection regression.}
#'
#' @noRd
calculate_p_tilde_kl <- function(data, control_vars) {
  residual_vars <- paste0(control_vars, "_cohort_time_average_residuals")
  
  rhs <- paste(residual_vars, collapse = " + ")
  formula <- as.formula(paste("instrument_residuals ~", rhs, "-1"))
  
  fit_p_tilde_kl <- lm(formula, data = data)
  data$p_tilde_kl <- predict(fit_p_tilde_kl)
  r_squared <- summary(fit_p_tilde_kl)$r.squared

  r_list <- list(data = data, r_squared = r_squared)
  return(r_list)
}

#' Calculate the variance of (p_tilde_kl - p_tilde).
#'
#' @param data A data.frame. 
#'
#' @return A list with:
#'   \item{data}{A data.frame with new variables p_tilde and zp.}
#'   \item{V_b_kl_zp}{Numeric. The variance of zp.}
#' 
#' @noRd
calculate_V_b_kl_zp <- function(data) {
  data$p_tilde <- residuals(lm(projection_cohort_time ~ factor(id) + factor(time), data = data))
  
  data$zp <- data$p_tilde_kl - data$p_tilde
  
  N <- nrow(data)
  V_b_kl_zp <- var(data$zp) * (N-1) / N
  
  r_list <- list(data = data, V_b_kl_zp = V_b_kl_zp)
  return(r_list)
}

#' Calculate the between coefficient by regressing a given variable on (p_tilde_kl - p_tilde) with unit and time fixed effects.
#'
#' @param data A data.frame. 
#' @param variable A string, the dependent variable name ("outcome", "treatment", etc.).
#'
#' @return A numeric value representing the between coefficient for zp.
#'
#' @noRd
calculate_between_kl_zp <- function(data, variable) {
  formula <- as.formula(paste(variable, "~ zp + factor(time) + factor(id)"))
  between_kl_zp <- lm(formula, data = data)$coef["zp"]
  return(between_kl_zp)
}

#' Calculate the between coefficinet in (k,l) subsample by regressing a given variable on instrument and cohort_time_average variables with unit and time fixed effects.
#'
#' @param data A data.frame. 
#' @param control_vars A character vector of original control variable names.
#' @param variable A string, the dependent variable name ("outcome", "treatment", etc.).
#'
#' @return A numeric value representing between_kl_outcome.
#'
#' @noRd
calculate_between_kl <- function(data, control_vars, variable) {
  control_average_vars <- paste0(control_vars, "_cohort_time_average")
  
  rhs_controls <- paste(control_average_vars, collapse = " + ")
  
  formula <- as.formula(paste0(
    variable, " ~ instrument + factor(time) + factor(id) + ", rhs_controls
  ))
  
  between_kl <- lm(formula, data = data)$coef["instrument"]
  return(between_kl)
}

#' Calculate the between IV coefficient in each cohort combination.
#'
#' @param r_squared Numeric. R-squared from the projection of instrument_residuals
#'   on the cohort-time average residualized controls.
#' @param V_Z Numeric. The variance of the residualized instrument.
#' @param between_kl_outcome Numeric. The coefficient on instrument from regressing 
#'   outcome on instrument, controls, and two-way fixed effects in the (k,l) subsample.
#' @param V_b_kl_zp Numeric. The variance of p_tilde_kl - p_tilde.
#' @param between_kl_zp_outcome Numeric. The coefficient on (p_tilde_kl - p_tilde) 
#'   from regressing outcome on zp with unit and time fixed effects.
#' @param between_kl_treatment Numeric. The coefficient on instrument from regressing 
#'   treatment on instrument, controls, and two-way fixed effects in the (k,l) subsample.
#' @param between_kl_zp_treatment Numeric. The coefficient on (p_tilde_kl - p_tilde) 
#'   from regressing treatment on zp with unit and time fixed effects. 
#'
#' @return A numeric scalar representing the between IV coefficient for the (k,l) cohort combination.
#'
#' @noRd
calculate_between_IV_coefficient_kl <- function(r_squared, V_Z, between_kl_outcome, V_b_kl_zp, between_kl_zp_outcome, between_kl_treatment, between_kl_zp_treatment) {
  between_IV_coefficient_kl <- ((1 - r_squared) * V_Z * between_kl_outcome + V_b_kl_zp * between_kl_zp_outcome)/((1 - r_squared) * V_Z * between_kl_treatment + V_b_kl_zp * between_kl_zp_treatment)
  return(between_IV_coefficient_kl)
}

#' Calculate the weight in each cohort combination.
#'
#' @param N Numeric. The number of observations.
#' @param r_squared Numeric. R-squared from the projection of instrument_residuals 
#'   on the cohort-time average residualized controls.
#' @param V_Z Numeric. The variance of the residualized instrument.
#' @param V_b_kl_zp Numeric. The variance of p_tilde_kl - p_tilde.
#' @param between_kl_treatment Numeric. The coefficient on instrument from regressing 
#'   treatment on instrument, controls, and two-way fixed effects in the (k,l) subsample.
#' @param between_kl_zp_treatment Numeric. The coefficient on (p_tilde_kl - p_tilde) 
#'   from regressing treatment on zp with unit and time fixed effects.
#'
#' @return A numeric scalar representing the weight for the (k,l) cohort combination.
#'
#' @noRd
calculate_weight_kl <- function(N, r_squared, V_Z, V_b_kl_zp, between_kl_treatment, between_kl_zp_treatment) {
  weight_kl <- N^2 * ((1 - r_squared) * V_Z * between_kl_treatment + V_b_kl_zp * between_kl_zp_treatment)
  return(weight_kl)
}

#' Calculate the weight and between IV coefficient in each cohort combination.
#'
#' #' This function integrates the intermediate steps of the decomposition procedure
#' to compute, for a given (k,l) cohort subsample:
#' 
#' - the variance of the residualized instrument,
#' - the projection of the instrument residuals on cohort-time average controls,
#' - the variance of (p_tilde_kl - p_tilde),
#' - the between coefficients from outcome and treatment regressions,
#'
#' and then combines these components to produce:
#' 
#' - the between IV coefficient for the (k,l) subsample,
#' - the associated weight.
#'
#' @param data A data.frame. 
#' @param control_vars A character vector of original control variable names.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{weight_kl}{Numeric. The weight for the (k, l) cohort combination.}
#'   \item{between_IV_coefficient_kl}{Numeric. The between IV coefficient for the (k, l) cohort combination.}
#'
#' @noRd
calculate_weight_and_between_IV_coefficient_kl <- function(data, control_vars) {
  
  result_partial_out_instrument <- partial_out_instrument(data)
  data <- result_partial_out_instrument$data
  V_Z <- result_partial_out_instrument$V_Z

  data <- partial_out_cohort_time_covariate(data, control_vars)

  result_calculate_p_tilde_kl <- calculate_p_tilde_kl(data, control_vars)
  data <- result_calculate_p_tilde_kl$data
  r_squared <- result_calculate_p_tilde_kl$r_squared

  result_calculate_V_b_kl_zp <- calculate_V_b_kl_zp(data)
  V_b_kl_zp <- result_calculate_V_b_kl_zp$V_b_kl_zp
  data <- result_calculate_V_b_kl_zp$data

  between_kl_zp_outcome <- calculate_between_kl_zp(data, "outcome")
  between_kl_zp_treatment <- calculate_between_kl_zp(data, "treatment")

  between_kl_outcome <- calculate_between_kl(data, control_vars, "outcome")
  between_kl_treatment <- calculate_between_kl(data, control_vars, "treatment")

  N <- nrow(data)

  between_IV_coefficient_kl <- calculate_between_IV_coefficient_kl(r_squared, V_Z, between_kl_outcome, V_b_kl_zp, between_kl_zp_outcome, between_kl_treatment, between_kl_zp_treatment)
  weight_kl <- calculate_weight_kl(N, r_squared, V_Z, V_b_kl_zp, between_kl_treatment, between_kl_zp_treatment)

  r_list <- list(weight_kl = weight_kl, between_IV_coefficient_kl = between_IV_coefficient_kl)
  return(r_list)
}













                          