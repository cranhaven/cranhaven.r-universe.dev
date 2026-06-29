library(testthat)
test_that("test-usage_2", {
    # parallel on
    model <- bkt(seed = 42, num_fits = 1, parallel = TRUE)
    fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")
    # fit(model, data_path = "ct.csv", skills = ".*Plot.*")
})
