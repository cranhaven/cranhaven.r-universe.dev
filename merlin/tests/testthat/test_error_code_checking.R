testthat::context("error_code_checking")

### necessary inputs

# Model must be specified
testthat::test_that("Test 1.1", {
  testthat::expect_error(
    object = merlin::merlin(
      levels = c("id"),
      family = "gaussian",
      data = pbc.merlin
    ),
    regexp = "argument \"model\" is missing, with no default"
  )
})

# Data must be passed to merlin
testthat::test_that("Test 1.2", {
  testthat::expect_error(
    object = merlin::merlin(
      model = list(logb ~ time + M1[id] * 1),
      levels = c("id"),
      family = "gaussian"
    ),
    regex = "argument \"data\" is missing, with no default"
  )
})

### error codes

# Number of families specified must equal the number of models
testthat::test_that("Test 2.1", {
  testthat::expect_error(
    object = merlin::merlin(
      model = list(
        logb ~ M1[id] * 1,
        logp ~ M2[id] * 1
      ),
      family = c("gaussian"),
      levels = c("id"),
      covariance = "identity",
      data = pbc.merlin
    ),
    regex = "The number of families specified must equal the number of models"
  )
})

test_that("Test 2.2", { # it won't really allow for this right now
  errormessage <- "region has not been included in the levels option"
  expect_error(merlin(model=list(logp ~ M1[id]*1,
                                 logb ~ M2[region]*1),
                      family=c("gaussian","gaussian"),
                      levels=c("id"),
                      covariance="identity",
                      data=pbc.merlin),
               errormessage)
})

# Specified levels must refer to a column in data
testthat::test_that("Test 2.3", {
  testthat::expect_error(
    merlin::merlin(
      model = list(
        logb ~ M1[id] * 1,
        logp ~ M2[id] * 1
      ),
      family = c("gaussian", "gaussian"),
      levels = c("id2"),
      covariance = "identity",
      data = pbc.merlin
    ),
    regex = "The specified levels must refer to a column in the dataset"
  )
})

# Passing a wrong family name triggers an error
testthat::test_that("Test 2.4", {
  testthat::expect_error(
    merlin::merlin(
      model = list(
        logb ~ M1[id] * 1,
      ),
      family = "noname",
      levels = "id",
      covariance = "identity",
      data = pbc.merlin
    ),
    regex = "'arg' should be one of "
  )
  testthat::expect_error(
    merlin::merlin(
      model = list(
        logb ~ M1[id] * 1,
        logp ~ M2[id] * 1
      ),
      family = c("hey", "gaussian"),
      levels = c("id"),
      covariance = "identity",
      data = pbc.merlin
    ),
    regex = "The number of families specified must equal the number of models"
  )
})

# Outcome variable must be present in the dataset
testthat::test_that("Test 2.5", {
  errormessage <- "argument \"family\" is missing, with no default"
  testthat::expect_error(
    object = merlin::merlin(
      model = list(y ~ x + M1[id] * 1),
      levels = c("id"),
      data = pbc.merlin
    ),
    regex = "Outcome variable y is not present in data set"
  )
  testthat::expect_error(
    merlin::merlin(
      model = list(
        logq ~ M1[id] * 1,
        logp ~ M2[id] * 1
      ),
      family = c("gaussian", "gaussian"),
      levels = c("id"),
      covariance = "identity",
      data = pbc.merlin
    ),
    regex = "Outcome variable logq is not present in data set"
  )
})

# Valid covariance structure
testthat::test_that("Test 2.6", {
  testthat::expect_error(
    merlin::merlin(
      model = list(logb ~ time + M1[id]@c),
      levels = c("id"),
      timevar = c("time"),
      family = "gaussian",
      covariance = "unstructued",
      data = pbc.merlin
    ),
    regex = "\"unstructued\" is not a valid covariance structure"
  )
  testthat::expect_error(
    merlin::merlin(
      model = list(logb ~ time + M1[id]@c),
      levels = c("id"),
      timevar = c("time"),
      family = "gaussian",
      covariance = "hello",
      data = pbc.merlin
    ),
    regex = "\"hello\" is not a valid covariance structure"
  )
})

# Specified weights must refer to a column in the dataset
testthat::test_that("Test 2.7", {
  testthat::expect_error(
    merlin::merlin(
      model = list(logb ~ time + M1[id]@c),
      levels = c("id"),
      timevar = c("time"),
      family = "gaussian",
      covariance = "unstructured",
      sweights = "wt1",
      data = pbc.merlin
    ),
    regex = "The specified weights must refer to a column in the dataset"
  )
})

# Using EV requires square brackets
testthat::test_that("Test 2.8a", {
  testthat::expect_error(
    merlin::merlin(
      model = list(
        Surv(stime, died) ~ EV(logb) + M1[id],
        logb ~ year + M1[id] * 1
      ),
      family = c("weibull", "gaussian"),
      levels = c("id"),
      covariance = "identity",
      timevar = c("stime", "year"),
      data = pbc.merlin
    ),
    regex = "Using EV requires square brackets"
  )
})

# Argument of EV must be a specified model outcome
testthat::test_that("Test 2.8b", {
  testthat::expect_error(
    merlin::merlin(
      model = list(
        Surv(stime, died) ~ EV[logp] + M1[id],
        logb ~ year + M1[id] * 1
      ),
      family = c("weibull", "gaussian"),
      levels = c("id"),
      covariance = "identity",
      timevar = c("stime", "year"),
      data = pbc.merlin
    ),
    regex = "logp is not a specified model outcome"
  )
})

# Expected value of an outcome cannot be used in the same model
testthat::test_that("Test 2.8c", {
  testthat::expect_error(
    merlin::merlin(
      model = list(
        Surv(stime, died) ~ EV[logb] + M1[id],
        logb ~ EV[logb] + year + M1[id] * 1
      ),
      family = c("weibull", "gaussian"),
      levels = c("id"),
      covariance = "identity",
      timevar = c("stime", "year"),
      data = pbc.merlin
    ),
    regex = "The expected value of an outcome variable cannot be used in the model for that outcome"
  )
})

# Outcome variables must be present in dataset
testthat::test_that("Test 2.9", {
  testthat::expect_error(
    merlin::merlin(
      model = list(
        Surv(stime2, died) ~ EV[logb] + M1[id],
        logb ~ year + M1[id] * 1
      ),
      family = c("weibull", "gaussian"),
      levels = c("id"),
      covariance = "identity",
      timevar = c("stime", "year"),
      data = pbc.merlin
    ),
    regex = "Outcome variable stime2 is not present in data set"
  )
  testthat::expect_error(
    merlin::merlin(
      model = list(
        Surv(stime, died2) ~ EV[logb] + M1[id],
        logb ~ year + M1[id] * 1
      ),
      family = c("weibull", "gaussian"),
      levels = c("id"),
      covariance = "identity",
      timevar = c("stime", "year"),
      data = pbc.merlin
    ),
    regex = "Outcome variable died2 is not present in data set"
  )
  testthat::expect_error(
    merlin::merlin(
      model = list(
        Surv(stime, died) ~ EV[logb] + M1[id],
        logb2 ~ year + M1[id] * 1
      ),
      family = c("weibull", "gaussian"),
      levels = c("id"),
      covariance = "identity",
      timevar = c("stime", "year"),
      data = pbc.merlin
    ),
    regex = "Outcome variable logb2 is not present in data set"
  )
})

# Outcome must be of appropriate type
testthat::test_that("Test 2.10a", {
  testthat::expect_error(
    merlin::merlin(
      model = list(
        Surv(stime, died) ~ EV[logb] + M1[id],
        status ~ year + M1[id] * 1
      ),
      family = c("weibull", "gaussian"),
      levels = c("id"),
      covariance = "identity",
      timevar = c("stime", "year"),
      data = pbc.merlin
    ),
    regex = "Outcome must be numeric"
  )
  testthat::expect_error(
    merlin::merlin(
      model = list(
        Surv(stime, status) ~ EV[logb] + M1[id],
        logb ~ year + M1[id] * 1
      ),
      family = c("weibull", "gaussian"),
      levels = c("id"),
      covariance = "identity",
      timevar = c("stime", "year"),
      data = pbc.merlin
    ),
    regex = "Event indicator must be numeric"
  )
  testthat::expect_error(
    merlin::merlin(
      model = list(
        Surv(stime, year) ~ EV[logb] + M1[id],
        logb ~ year + M1[id] * 1
      ),
      family = c("weibull", "gaussian"),
      levels = c("id"),
      covariance = "identity",
      timevar = c("stime", "year"),
      data = pbc.merlin
    ),
    regex = "Event indicator must be 0 or 1"
  )
  testthat::expect_error(
    merlin::merlin(
      model = list(
        Surv(stime, status) ~ EV[logb] + M1[id],
        logb ~ year + M1[id] * 1
      ),
      family = c("weibull", "gaussian"),
      levels = c("id"),
      covariance = "identity",
      timevar = c("stime", "year"),
      data = pbc.merlin
    ),
    regex = "Event indicator must be numeric"
  )
  testthat::expect_error(
    merlin::merlin(
      model = list(
        Surv(stime, year) ~ EV[logb] + M1[id],
        logb ~ year + M1[id] * 1
      ),
      family = c("weibull", "gaussian"),
      levels = c("id"),
      covariance = "identity",
      timevar = c("stime", "year"),
      data = pbc.merlin
    ),
    regex = "Event indicator must be 0 or 1"
  )
})

# timevar must be specified
testthat::test_that("Test 2.11", {
  testthat::expect_error(
    merlin::merlin(
      model = list(
        Surv(stime, died) ~ EV[logb] + M1[id],
        logb ~ year + M1[id] * 1
      ),
      family = c("weibull", "gaussian"),
      levels = c("id"),
      covariance = "identity",
      data = pbc.merlin
    ),
    regex = "timevar option must be specified for this model"
  )
})

# If random effects are specified, levels must be specified too
testthat::test_that("Test 2.12a", {
  testthat::expect_error(
    merlin::merlin(
      model = list(
        Surv(stime, died) ~ EV[logb] + M1[id],
        logb ~ year + M1[id] * 1
      ),
      family = c("weibull", "gaussian"),
      covariance = "identity",
      timevar = c("stime", "year"),
      data = pbc.merlin
    ),
    regex = "If random-effects are specified the levels option must be used"
  )
  testthat::expect_error(
    merlin::merlin(
      model = list(
        Surv(stime, died) ~ EV[logb] + M1[id],
        logb ~ year + M1[trt] * 1
      ),
      family = c("weibull", "gaussian"),
      covariance = "identity",
      timevar = c("stime", "year"),
      levels = c("id"),
      data = pbc.merlin
    ),
    regex = "trt has not been included in the levels option"
  )
})
