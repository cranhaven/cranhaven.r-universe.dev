context('Test wrapper function implementing drug prep algorithm')

nonnegative <- data.frame(prodcode = c('a', 'b'),
                          min_qty = 0,   min_ndd = 0,
                          max_qty = Inf, max_ndd = Inf,
                          stringsAsFactors = FALSE)

test_that('Picking all "default" decisions leaves data unchanged', {
  decisions <- rep('a', 10)

  # 'numdays' needs to be present in input for the default for decision 6
  example_therapy <- transform(example_therapy, numdays = qty / ndd)
  result <- drug_prep(example_therapy, nonnegative, decisions)
  expect_equivalent(subset(result, select = c(-duration, -stop_date)),
                    example_therapy)

  # Or pick decision 6 = 'c' (qty / ndd) and then 'numdays' is not needed
  example_therapy$numdays <- NULL
  decisions[6] <- 'c'
  result <- drug_prep(example_therapy, nonnegative, decisions)
  expect_equivalent(subset(result, select = c(-duration, -stop_date)),
                    example_therapy)
})

test_that('Try an arbitrary example decision set', {
  decisions <- c('b', 'b1', 'b', 'b1', 'b_6',
                 'c', 'a', 'd', 'a', 'b_15')
  result <- drug_prep(example_therapy, nonnegative, decisions)
  expect_false(any(sapply(result, function(x) all(is.na(x)))))
})
