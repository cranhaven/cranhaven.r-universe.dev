#' Get and Set Contact Matrix
#'
#' These functions allow getting and setting the contact matrix for
#' measles mixing models. The contact matrix specifies the mixing patterns
#' between different population groups.
#'
#' @param model An epiworld model object of class `epiworld_measlesmixing` or
#'   `epiworld_measlesmixingriskquarantine`.
#' @param value A row-stochastic matrix representing contact rates between
#'   population groups. The matrix should be square with dimensions matching
#'   the number of entities (population groups) in the model, and each row
#'   should sum to 1.
#'
#' @return
#' - `get_contact_matrix()` returns a numeric matrix representing the contact
#'   rates between population groups.
#' - `set_contact_matrix()` returns the model object invisibly (called for
#'   its side effects).
#'
#' @details
#' The contact matrix is a row-stochastic matrix where entry `[i, j]`
#' represents the probability that an individual in group `i` will make
#' contact with someone in group `j`, given that they make a contact.
#' Each row must sum to 1.
#'
#' These functions are currently only available for:
#' - [ModelMeaslesMixing]
#' - [ModelMeaslesMixingRiskQuarantine]
#'
#' Other mixing models in epiworld will have these methods available in the
#' near future.
#'
#' @examples
#' # Create entities for three population groups
#' e1 <- entity("Population 1", 1000, as_proportion = FALSE)
#' e2 <- entity("Population 2", 1000, as_proportion = FALSE)
#' e3 <- entity("Population 3", 1000, as_proportion = FALSE)
#'
#' # Create an identity contact matrix (no mixing between groups)
#' cmatrix <- diag(3)
#'
#' N <- 3000
#'
#' # Create a measles mixing model
#' model <- ModelMeaslesMixing(
#'   n                        = N,
#'   prevalence               = 1 / N,
#'   contact_rate             = 15,
#'   transmission_rate        = 0.9,
#'   vax_efficacy             = 0.97,
#'   vax_reduction_recovery_rate = 0.8,
#'   incubation_period        = 10,
#'   prodromal_period         = 3,
#'   rash_period              = 7,
#'   contact_matrix           = cmatrix,
#'   hospitalization_rate     = 0.1,
#'   hospitalization_period   = 10,
#'   days_undetected          = 2,
#'   quarantine_period        = 14,
#'   quarantine_willingness   = 0.9,
#'   isolation_willingness    = 0.8,
#'   isolation_period         = 10,
#'   prop_vaccinated          = 0.95,
#'   contact_tracing_success_rate = 0.8,
#'   contact_tracing_days_prior = 4
#' )
#'
#' # Add entities to the model
#' model |>
#'   add_entity(e1) |>
#'   add_entity(e2) |>
#'   add_entity(e3)
#'
#' # Get the contact matrix (note: requires running the model first)
#' set.seed(123)
#' run(model, ndays = 10)
#' original_matrix <- get_contact_matrix(model)
#' print(original_matrix)
#'
#' # Create a new random row-stochastic matrix
#' new_matrix <- matrix(
#'   c(0.8, 0.1, 0.1,
#'     0.1, 0.7, 0.2,
#'     0.15, 0.15, 0.7),
#'   nrow = 3, byrow = TRUE
#' )
#'
#' # Set the new contact matrix
#' set_contact_matrix(model, new_matrix)
#'
#' # Verify the change
#' updated_matrix <- get_contact_matrix(model)
#' print(updated_matrix)
#'
#' @name contact_matrix
#' @aliases get_contact_matrix set_contact_matrix
#' @export
get_contact_matrix <- function(model) {
  UseMethod("get_contact_matrix")
}

#' @rdname contact_matrix
#' @export
get_contact_matrix.default <- function(model) {
  stop(
    "get_contact_matrix() is not available for this model type. ",
    "Currently supported: epiworld_measlesmixing, ",
    "epiworld_measlesmixingriskquarantine",
    call. = FALSE
  )
}

#' @rdname contact_matrix
#' @export
get_contact_matrix.epiworld_measlesmixing <- function(model) {
  stopifnot_model(model)

  # Get the vector from C++ and convert to matrix
  # C++ stores the matrix in column-major order (same as R)
  vec <- get_contact_matrix_mixing_cpp(model)
  n <- sqrt(length(vec))
  matrix(vec, nrow = n, ncol = n)
}

#' @rdname contact_matrix
#' @export
get_contact_matrix.epiworld_measlesmixingriskquarantine <- function(model) {
  stopifnot_model(model)

  # Get the vector from C++ and convert to matrix
  # C++ stores the matrix in column-major order (same as R)
  vec <- get_contact_matrix_mixing_risk_quarantine_cpp(model)
  n <- sqrt(length(vec))
  matrix(vec, nrow = n, ncol = n)
}

#' @rdname contact_matrix
#' @export
set_contact_matrix <- function(model, value) {
  UseMethod("set_contact_matrix")
}

#' @rdname contact_matrix
#' @export
set_contact_matrix.default <- function(model, value) {
  stop(
    "set_contact_matrix() is not available for this model type. ",
    "Currently supported: epiworld_measlesmixing, ",
    "epiworld_measlesmixingriskquarantine",
    call. = FALSE
  )
}

#' @rdname contact_matrix
#' @export
set_contact_matrix.epiworld_measlesmixing <- function(model, value) {
  stopifnot_model(model)

  # Validate the matrix: must be numeric and contain probabilities [0, 1]
  if (!is.matrix(value) || !is.numeric(value)) {
    stop("value must be a numeric matrix")
  }

  # Check that all values are probabilities (between 0 and 1)
  stopifnot_double(as.vector(value), lb = 0, ub = 1)

  # as.vector() converts matrix to column-major order (same as C++ expects)
  set_contact_matrix_mixing_cpp(model, as.vector(value))
  invisible(model)
}

#' @rdname contact_matrix
#' @export
set_contact_matrix.epiworld_measlesmixingriskquarantine <- function(model, value) {
  stopifnot_model(model)

  # Validate the matrix: must be numeric and contain probabilities [0, 1]
  if (!is.matrix(value) || !is.numeric(value)) {
    stop("value must be a numeric matrix")
  }

  # Check that all values are probabilities (between 0 and 1)
  stopifnot_double(as.vector(value), lb = 0, ub = 1)

  # as.vector() converts matrix to column-major order (same as C++ expects)
  set_contact_matrix_mixing_risk_quarantine_cpp(model, as.vector(value))
  invisible(model)
}
