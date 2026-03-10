# Test just this file: tinytest::run_test_file("inst/tinytest/test-contact-matrix.R")

# Helper function to create a random row-stochastic matrix
create_random_row_stochastic_matrix <- function(n) {
  mat <- matrix(runif(n * n), nrow = n, ncol = n)
  # Normalize rows to sum to 1
  mat <- mat / rowSums(mat)
  return(mat)
}

# ------------------------------------------------------------------------------
# Test get_contact_matrix() and set_contact_matrix() for ModelMeaslesMixing
# ------------------------------------------------------------------------------

# Create entities for three population groups
e1 <- entity("Population 1", 1000, FALSE)
e2 <- entity("Population 2", 1000, FALSE)
e3 <- entity("Population 3", 1000, FALSE)

# (1) Initialize the model with an identity matrix for the mixing matrix
identity_matrix <- diag(3)

N <- 3000

model_mixing <- measles::ModelMeaslesMixing(
  n                          = N,
  prevalence                 = 1 / N,
  contact_rate               = 15,
  transmission_rate          = 0.9,
  vax_efficacy               = 0.97,
  vax_reduction_recovery_rate = 0.8,
  incubation_period          = 10,
  prodromal_period           = 3,
  rash_period                = 7,
  contact_matrix             = identity_matrix,
  hospitalization_rate       = 0.1,
  hospitalization_period     = 10,
  days_undetected            = 2,
  quarantine_period          = 14,
  quarantine_willingness     = 0.9,
  isolation_willingness      = 0.8,
  isolation_period           = 10,
  prop_vaccinated            = 0.95,
  contact_tracing_success_rate = 0.8,
  contact_tracing_days_prior = 4
)

# Add entities to the model
model_mixing |>
  add_entity(e1) |>
  add_entity(e2) |>
  add_entity(e3)

# Expecting error
expect_error(
  set_contact_matrix(model_mixing, matrix(c(-0.1, 0.5, 0.6, 0.5), 2, 2)),
  "must be greater than or equal to 0"
)

# Run the model first so contact matrix is initialized
set.seed(123)
run(model_mixing, ndays = 10)

# (2) Extract the matrix using get_contact_matrix() and check dimensions
extracted_matrix <- get_contact_matrix(model_mixing)
expect_equal(dim(extracted_matrix), c(3, 3))

# (3) Create a random row-stochastic matrix of the same dimension
set.seed(456)
random_matrix <- create_random_row_stochastic_matrix(3)

# Set it using set_contact_matrix()
set_contact_matrix(model_mixing, random_matrix)

# (4) Extract the matrix and compare with the one used in set_contact_matrix
extracted_after_set <- get_contact_matrix(model_mixing)
expect_equal(extracted_after_set, random_matrix, tolerance = 1e-10)

# ------------------------------------------------------------------------------
# Test get_contact_matrix() and set_contact_matrix() for
# ModelMeaslesMixingRiskQuarantine
# ------------------------------------------------------------------------------

# Create new entities
e1_rq <- entity("Population 1", 1000, FALSE)
e2_rq <- entity("Population 2", 1000, FALSE)
e3_rq <- entity("Population 3", 1000, FALSE)

# (1) Initialize the model with an identity matrix
model_risk_quar <- measles::ModelMeaslesMixingRiskQuarantine(
  n                          = N,
  prevalence                 = 1 / N,
  contact_rate               = 15,
  transmission_rate          = 0.9,
  vax_efficacy               = 0.97,
  incubation_period          = 10,
  prodromal_period           = 3,
  rash_period                = 7,
  contact_matrix             = identity_matrix,
  hospitalization_rate       = 0.1,
  hospitalization_period     = 10,
  days_undetected            = 2,
  quarantine_period_high     = 21,
  quarantine_period_medium   = 14,
  quarantine_period_low      = 7,
  quarantine_willingness     = 0.9,
  isolation_willingness      = 0.8,
  isolation_period           = 10,
  prop_vaccinated            = 0.95,
  detection_rate_quarantine  = 0.5,
  contact_tracing_success_rate = 0.8,
  contact_tracing_days_prior = 4
)

# Add entities to the model
model_risk_quar |>
  add_entity(e1_rq) |>
  add_entity(e2_rq) |>
  add_entity(e3_rq)

# Expecting error
expect_error(
  set_contact_matrix(model_risk_quar, matrix(c(-0.1, 0.5, 0.6, 0.5), 2, 2)),
  "must be greater than or equal to 0"
)

# Run the model first so contact matrix is initialized
set.seed(789)
run(model_risk_quar, ndays = 10)

# (2) Extract the matrix and check dimensions
extracted_matrix_rq <- get_contact_matrix(model_risk_quar)
expect_equal(dim(extracted_matrix_rq), c(3, 3))

# (3) Create a random row-stochastic matrix of the same dimension
set.seed(101112)
random_matrix_rq <- create_random_row_stochastic_matrix(3)

# Set it using set_contact_matrix()
set_contact_matrix(model_risk_quar, random_matrix_rq)

# (4) Extract and compare
extracted_after_set_rq <- get_contact_matrix(model_risk_quar)
expect_equal(extracted_after_set_rq, random_matrix_rq, tolerance = 1e-10)

# ------------------------------------------------------------------------------
# Test default method returns error for unsupported models
# ------------------------------------------------------------------------------

# Test with an arbitrary object
expect_error(
  get_contact_matrix(list(a = 1)),
  "get_contact_matrix\\(\\) is not available for this model type"
)

expect_error(
  set_contact_matrix(list(a = 1), diag(3)),
  "set_contact_matrix\\(\\) is not available for this model type"
)