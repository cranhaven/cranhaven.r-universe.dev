context('Test features of the proposed new user interface')

test_that('Example dataset loads correctly', {
  expect_equal(nrow(example_therapy), 30L)
  expect_equal(colnames(example_therapy),
               c('patid', 'pracid', 'prodcode', 'start_date', 'qty', 'ndd'))
})

test_that('Replacing missing quantities with NA is equivalent to identity', {
  expect_equivalent(impute_qty(example_therapy, 'ignore'),
                    identity(example_therapy))
  expect_equivalent(impute_qty(example_therapy, 'replace'),
                    identity(example_therapy))
  expect_equivalent(impute_qty(example_therapy, 'ignore', function(x) TRUE),
                    identity(example_therapy))
  expect_equivalent(impute_qty(example_therapy, 'ignore', function(x) rep(TRUE, length(x))),
                    identity(example_therapy))
})

test_that('Replacing missing numerical daily doses with NA is equivalent to identity', {
  expect_equivalent(impute_ndd(example_therapy, 'ignore'),
                    identity(example_therapy))
  expect_equivalent(impute_ndd(example_therapy, 'replace'),
                    identity(example_therapy))
  expect_equivalent(impute_ndd(example_therapy, 'ignore', function(x) TRUE),
                    identity(example_therapy))
  expect_equivalent(impute_ndd(example_therapy, 'ignore', function(x) rep(TRUE, length(x))),
                    identity(example_therapy))
  expect_equivalent(impute_ndd(example_therapy, 'ignore', function(x) runif(length(x)) > .3),
                    identity(example_therapy))
})

test_that('Impute function does what it says on the tin', {
  library(dplyr)
  expect_equivalent(impute_qty(example_therapy, 'mean'),
                    example_therapy %>%
                      group_by(prodcode) %>%
                      mutate(qty = ifelse(is.na(qty), mean(qty, na.rm = TRUE), qty)))
  expect_equivalent(impute_ndd(example_therapy, 'mean'),
                    example_therapy %>%
                      group_by(prodcode) %>%
                      mutate(ndd = ifelse(is.na(ndd), mean(ndd, na.rm = TRUE), ndd)))
  expect_equivalent(impute_qty(example_therapy, 'median'),
                    example_therapy %>%
                      group_by(prodcode) %>%
                      mutate(qty = ifelse(is.na(qty), median(qty, na.rm = TRUE), qty)))
  expect_equivalent(impute_ndd(example_therapy, 'median'),
                    example_therapy %>%
                      group_by(prodcode) %>%
                      mutate(ndd = ifelse(is.na(ndd), median(ndd, na.rm = TRUE), ndd)))
  expect_equivalent(impute_ndd(example_therapy, 'mode'),
                    example_therapy %>%
                      group_by(prodcode) %>%
                      mutate(ndd = ifelse(is.na(ndd), drugprepr:::get_mode(ndd), ndd)))
  expect_equivalent(impute_qty(example_therapy, 'mode'),
                    example_therapy %>%
                      group_by(prodcode) %>%
                      mutate(qty = ifelse(is.na(qty), drugprepr:::get_mode(qty), qty)))
})

test_that('Clean overlong prescription durations', {
  long_presc <- dplyr::tibble(duration = c(100, 300, 400, 800))
  expect_equal(clean_duration(long_presc), long_presc)
  expect_equal(clean_duration(long_presc, 6),
               dplyr::tibble(duration = c(100, 182, 182, 182)))
  expect_equal(clean_duration(long_presc, 12, 'remove'),
               dplyr::tibble(duration = c(100, 300, NA, NA)))
})

test_that('Imputing missing prescription durations', {
  library(dplyr)
  example_duration <- transform(example_therapy, duration = qty / ndd)
  expect_equal(impute_duration(example_duration, method = 'ignore'),
               dplyr::as_tibble(example_duration))
  expect_equal(impute_duration(example_duration, method = 'replace'),
               dplyr::as_tibble(example_duration))
  expect_equal(impute_duration(example_duration, method = 'mean'),
               example_duration %>%
                 group_by(prodcode, patid, start_date) %>%
                 mutate(duration = ifelse(is.na(duration),
                                          mean(duration, na.rm = T),
                                          duration)) %>%
                 ungroup())
})

test_that('Closing zero-width gaps is same as doing nothing', {
  example_gaps <- transform(example_therapy, stop_date = start_date + qty / ndd)
  expect_equivalent(close_small_gaps(example_gaps), example_gaps)
})

test_that('Negative-width small gap throws an error', {
  example_gaps <- transform(example_therapy, stop_date = start_date + qty / ndd)
  expect_error(close_small_gaps(example_gaps, -1))
})
