# tests/testthat/helper.R
# Shared setup for testthat tests — loaded automatically by testthat

# Suppress brms/Stan messages during tests
options(brms.backend = "rstan")
