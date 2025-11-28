test_that("one factor works", {

  data("bfi", package="ufs");
  items <- names(bfi)[1:25];

  testResult <-
    rosetta::factorAnalysis(
      data = bfi,
      items = items,
      nfactors = 1,
      summary=TRUE,
      correlations = TRUE,
      colorLoadings = TRUE,
      scree = TRUE
    );

  print(testResult);

  testthat::expect_equal(
    testResult$output$n,
    2436
  );

})

test_that("two factors work", {

  data("bfi", package="ufs");
  items <- names(bfi)[1:25];

  testResult <-
    rosetta::factorAnalysis(
      data = bfi,
      items = items,
      nfactors = 2,
      summary=TRUE,
      correlations = TRUE,
      colorLoadings = TRUE,
      scree = TRUE
    );

  print(testResult);

  testthat::expect_equal(
    testResult$output$n,
    2436
  );

})
