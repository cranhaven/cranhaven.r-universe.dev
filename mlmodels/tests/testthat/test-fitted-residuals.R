# tests/testthat/test-fitted-residuals.R
library(testthat)

# Load data once
data(mroz)
data(smoke)
data(docvis)
data(pw401k)

mroz$incthou <- mroz$faminc / 1000
smoke$smokes <- smoke$cigs > 0

test_that("fitted() and residuals() work across all model types", {
  
  # Linear models
  suppressMessages({
    lm_hom <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, data = mroz)
    lm_het <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem,
                    scale = ~ educ + exper, data = mroz)
    ln_hom <- ml_lm(log(incthou) ~ age + I(age^2) + huswage + educ + unem, data = mroz)
  })
  
  expect_equal(length(fitted(lm_hom)), nrow(mroz))
  expect_equal(length(residuals(lm_hom)), nrow(mroz))
  expect_equal(length(residuals(lm_hom, type = "pearson")), nrow(mroz))
  
  expect_equal(length(fitted(ln_hom)), nrow(mroz))
  expect_equal(length(residuals(ln_hom)), nrow(mroz))
  
  # Binary models
  suppressMessages({
    logit <- ml_logit(smokes ~ cigpric + income + age, data = smoke)
    probit <- ml_probit(smokes ~ cigpric + income + age, data = smoke)
  })
  
  expect_equal(length(fitted(logit)), nrow(smoke))
  expect_equal(length(residuals(logit)), nrow(smoke))
  expect_equal(length(residuals(logit, type = "pearson")), nrow(smoke))
  
  # Count models
  suppressMessages({
    pois <- ml_poisson(docvis ~ age + educyr, data = docvis)
    nb2  <- ml_negbin(docvis ~ age + educyr, data = docvis)
  })
  
  expect_equal(length(fitted(pois)), nrow(docvis))
  expect_equal(length(residuals(pois)), nrow(docvis))
  expect_equal(length(residuals(pois, type = "pearson")), nrow(docvis))
  
  # Gamma and Beta
  suppressMessages({
    gam  <- ml_gamma(faminc ~ hours + educ, data = mroz)
    beta <- ml_beta(prate ~ mrate + age, data = pw401k, subset = prate < 1)
  })
  
  expect_equal(length(fitted(gam)), nrow(mroz))
  expect_equal(length(residuals(gam, type = "pearson")), nrow(mroz))
  
  # Beta returns full length even when subsetted
  expect_equal(length(fitted(beta)), nrow(pw401k))
  expect_equal(length(residuals(beta, type = "pearson")), nrow(pw401k))
})

test_that("residuals() returns NAs for dropped observations", {
  beta <- suppressMessages(
    ml_beta(prate ~ mrate + age, data = pw401k, subset = prate < 1)
  )
  
  expect_true(any(is.na(fitted(beta))))
  expect_true(any(is.na(residuals(beta))))
  expect_true(any(is.na(residuals(beta, type = "pearson"))))
})