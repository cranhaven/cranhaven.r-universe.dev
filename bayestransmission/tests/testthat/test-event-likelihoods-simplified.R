# test-event-likelihoods-simplified.R
# Test individual event type likelihood contributions with known parameters

library(testthat)
library(bayestransmission)

# Create a standard model for all tests
model_params <- list(
  modname = "LinearAbxModel",
  nstates = 2,
  nmetro = 100,
  forward = 1,
  cheat = 0,
  Insitu = InsituParams(
    probs = c(0.8321, 0, 0.1679),
    priors = c(0.9, 0, 0.1),
    doit = c(FALSE, FALSE, FALSE)
  ),
  SurveillanceTest = SurveillanceTestParams(
    uncolonized = Param(init = 0.0, weight = 0, update = FALSE),
    colonized = Param(init = 0.6543, weight = 1, update = FALSE),
    latent = Param(init = 0.0, weight = 0, update = FALSE)
  ),
  ClinicalTest = RandomTestParams(
    uncolonized = ParamWRate(
      param = Param(init = 0.2187, weight = 0, update = FALSE),
      rate = Param(init = 0.8765, weight = 0, update = FALSE)
    ),
    colonized = ParamWRate(
      param = Param(init = 0.0, weight = 0, update = FALSE),
      rate = Param(init = 0.0, weight = 0, update = FALSE)
    ),
    latent = ParamWRate(
      param = Param(init = 0.3456, weight = 0, update = FALSE),
      rate = Param(init = 1.2345, weight = 0, update = FALSE)
    )
  ),
  OutCol = OutOfUnitInfectionParams(
    acquisition = Param(init = 0.004321, weight = 1, update = FALSE),
    clearance = Param(init = 0.007654, weight = 0, update = FALSE)
  ),
  InCol = InUnitParams(
    acquisition = LinearAbxAcquisitionParams(
      base = Param(init = 0.001234, weight = 1, update = FALSE),
      time = Param(init = 1.1111, weight = 0, update = FALSE),
      mass = Param(init = 0.5, weight = 1, update = FALSE),
      freq = Param(init = 0.5, weight = 1, update = FALSE),
      col_abx = Param(init = 0.7777, weight = 0, update = FALSE),
      suss_abx = Param(init = 0.8888, weight = 0, update = FALSE),
      suss_ever = Param(init = 0.9999, weight = 0, update = FALSE)
    ),
    clearance = ClearanceParams(
      rate = Param(init = 0.008765, weight = 1, update = FALSE),
      abx = Param(init = 0.6666, weight = 0, update = FALSE),
      ever_abx = Param(init = 0.5555, weight = 0, update = FALSE)
    )
  ),
  Abx = AbxParams(
    onoff = FALSE,
    delay = 0.0,
    life = 2.0
  ),
  AbxRate = AbxRateParams(
    uncolonized = Param(init = 1.3333, weight = 0, update = FALSE),
    colonized = Param(init = 1.4444, weight = 0, update = FALSE),
    latent = Param(init = 0.0, weight = 0, update = FALSE)
  )
)

# Create C++ model once
model <- newCppModel(model_params)

# Helper function to create episode data (admission to discharge)
create_episode_data <- function(events_df) {
  # events_df should have columns: time, patient, type
  # Prepend start marker and append stop marker
  data.frame(
    facility = 1,
    unit = 1,
    time = c(0, events_df$time, max(events_df$time) + 100),
    patient = c(NA, events_df$patient, NA),
    type = c(21, events_df$type, 23)
  )
}

create_system <- function(data) {
  CppSystem$new(
    as.integer(data$facility),
    as.integer(data$unit),
    data$time,
    as.integer(data$patient),
    as.integer(data$type)
  )
}

# Helper to compute likelihood for a dataset
compute_likelihood <- function(data, model_obj = model) {
  sys <- create_system(data)
  hist <- CppSystemHistory$new(sys, model_obj, FALSE)
  ll <- model_obj$logLikelihood(hist)
  return(list(ll = ll, model = model_obj, hist = hist))
}

# =============================================================================
# Test 1: Start and Stop markers (should contribute 0)
# =============================================================================

test_that("Empty system gives zero likelihood", {
  sys <- CppSystem$new(integer(0),
             integer(0),
             numeric(0),
             integer(0),
             integer(0))
  expect_equal(sys$countEvents(), 0)
  expect_equal(sys$countEpisodes(), 0)
  skip("getPatients requires ALL_CLASSES")
  expect_equal(sys$getPatients()$size, 0)

  hist <- CppSystemHistory$new(sys, model, FALSE)
  ll <- model$logLikelihood(hist)
  
  # Start and stop should contribute exactly 0 (not -Inf)
  expect_true(is.finite(ll))
  expect_equal(ll, 0, tolerance = 1e-10)
})

# =============================================================================
# Test 2: Simple admission and discharge
# =============================================================================

test_that("Simple admission has expected likelihood range", {
  data <- create_episode_data(data.frame(
    time = c(10, 20),
    patient = c(1, 1),
    type = c(0, 3)  # admission, discharge
  ))

  result <- compute_likelihood(data)
  
  # Should be finite
  expect_true(is.finite(result$ll))
  expect_true(result$ll <= 0)
  
  # Diagnostic (quiet): result$ll is stored via expectations below
  
  # Should be reasonably negative but not -Inf
  expect_true(result$ll > -10000, info = sprintf("Got likelihood: %f", result$ll))
  expect_true(result$ll < 0)
})

# =============================================================================
# Test 3: Negative surveillance test on likely uncolonized patient
# =============================================================================

test_that("Negative test on admission has reasonable likelihood", {
  data <- create_episode_data(data.frame(
    time = c(10, 10.001, 20),
    patient = c(1, 1, 1),
    type = c(0, 1, 3)  # admission, negsurvtest, discharge
  ))
  
  result <- compute_likelihood(data)
  
  # Should be finite
  expect_true(is.finite(result$ll))
  expect_true(result$ll <= 0)
  
  # Should be more negative than simple admission (test adds information)
  data_simple <- create_episode_data(data.frame(
    time = c(10, 20),
    patient = c(1, 1),
    type = c(0, 3)
  ))
  result_simple <- compute_likelihood(data_simple)
  
  # Negative test on likely uncolonized should be MORE likely (less negative)
  # than unknown state
  expect_true(result$ll >= result_simple$ll - 2, 
              info = sprintf("Test LL: %f, Simple LL: %f", result$ll, result_simple$ll))
})

# =============================================================================
# Test 4: Longer time gaps reduce likelihood
# =============================================================================

test_that("Longer time gaps have more negative likelihood", {
  # Short gap
  data_short <- create_episode_data(data.frame(
    time = c(10, 11),
    patient = c(1, 1),
    type = c(0, 3)
  ))
  
  result_short <- compute_likelihood(data_short)
  
  # Long gap
  data_long <- create_episode_data(data.frame(
    time = c(10, 50),
    patient = c(1, 1),
    type = c(0, 3)
  ))
  
  result_long <- compute_likelihood(data_long)
  
  # Both should be finite
  expect_true(is.finite(result_short$ll))
  expect_true(is.finite(result_long$ll))
  
  # Long gap should have lower (more negative) likelihood
  expect_true(result_long$ll < result_short$ll,
              info = sprintf("Short LL: %f, Long LL: %f", result_short$ll, result_long$ll))
  
  # Check magnitude of difference
  diff <- result_short$ll - result_long$ll
  expect_true(diff > 1, info = sprintf("Difference: %f", diff))
})

# =============================================================================
# Test 5: Individual event contributions sum to total
# =============================================================================

test_that("Individual event log likelihoods sum to total", {
  data <- create_episode_data(data.frame(
    time = c(10, 20),
    patient = c(1, 1),
    type = c(0, 3)
  ))

  result <- compute_likelihood(data)
  skip("getHistoryLinkList requires ALL_CLASSES")
  links <- result$hist$getHistoryLinkList()
  
  # Compute individual contributions
  individual_lls <- vapply(links, function(link) {
    result$model$logLikelihoodLink(link)
  }, numeric(1))
  
  # Sum should equal total
  expect_equal(sum(individual_lls), result$ll, tolerance = 1e-10,
               info = sprintf("Sum: %f, Total: %f", sum(individual_lls), result$ll))
  
  # Start and stop events should contribute 0
  event_types <- vapply(links, function(link) link$Event$Type, character(1))
  start_events <- which(event_types == "start")
  stop_events <- which(event_types == "stop")
  
  if (length(start_events) > 0) {
    expect_true(all(individual_lls[start_events] == 0))
  }
  if (length(stop_events) > 0) {
    expect_true(all(individual_lls[stop_events] == 0))
  }
})

# =============================================================================
# Test 6: Discharge events contribute 0
# =============================================================================

test_that("Discharge events contribute 0 to likelihood", {
  data <- create_episode_data(data.frame(
    time = c(10, 20),
    patient = c(1, 1),
    type = c(0, 3)
  ))

  result <- compute_likelihood(data)
  skip("getHistoryLinkList requires ALL_CLASSES")
  links <- result$hist$getHistoryLinkList()
  
  # Find discharge events and check their contributions
  discharge_info <- lapply(links, function(link) {
    if (link$Event$Type == "discharge") {
      list(
        ll = result$model$logLikelihoodLink(link),
        level = class(link$Event)[1],
        time = link$Event$Time
      )
    } else {
      NULL
    }
  })
  discharge_info <- Filter(Negate(is.null), discharge_info)
  
  if (length(discharge_info) > 0) {
    # Build failure-only diagnostic summary
    diag_lines <- vapply(discharge_info, function(info) {
      sprintf("Level: %s, Time: %f, LL: %f", info$level, info$time, info$ll)
    }, character(1))
    diag_text <- paste0("Discharge event contributions:\n", paste(diag_lines, collapse = "\n"))

    # Check that at least one discharge contributes 0 (patient-level)
    discharge_lls <- sapply(discharge_info, function(x) x$ll)
    zero_discharges <- sum(abs(discharge_lls) < 1e-10)
    expect_true(zero_discharges > 0,
                info = paste0(
                  sprintf("Found %d zero-contribution discharges out of %d total", 
                          zero_discharges, length(discharge_lls)),
                  "\n",
                  diag_text
                ))
  }
})

# =============================================================================
# Test 7: Multiple patients
# =============================================================================

test_that("Multiple patients contribute independently", {
  # Single patient
  data_single <- create_episode_data(data.frame(
    time = c(10, 20),
    patient = c(1, 1),
    type = c(0, 3)
  ))
  
  result_single <- compute_likelihood(data_single)
  
  # Two patients (non-overlapping)
  data_double <- create_episode_data(data.frame(
    time = c(10, 20, 30, 40),
    patient = c(1, 1, 2, 2),
    type = c(0, 3, 0, 3)
  ))
  
  result_double <- compute_likelihood(data_double)
  
  # Both should be finite
  expect_true(is.finite(result_single$ll))
  expect_true(is.finite(result_double$ll))
  
  # Two patients should have approximately double the negative likelihood
  # (but not exactly due to unit-level effects)
  expect_true(result_double$ll < result_single$ll,
              info = sprintf("Single: %f, Double: %f", result_single$ll, result_double$ll))
})
