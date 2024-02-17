
testthat::test_that('live query correctly returns data.frame', {
  
  testthat::skip_on_cran()
  
  testthat::expect_true(is.data.frame(
    get_data(
      resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
      fields = NULL,
      limit = NULL,
      where = NULL,
      page_size = NULL
    )
  ))
})


testthat::test_that('correct result returned using dummy httr::RETRY result', {
  
  load(testthat::test_path("test_data", "get_data_test_1.rda"))
  
  mockery::stub(get_data, 'httr::RETRY', function(...) get_data_test_1)
  
  testthat::expect_equal(
    digest::digest(get_data(
      resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
      fields = NULL,
      limit = NULL,
      where = NULL,
      page_size = NULL
    )),
    "5e7759e534e5cce9cbab55a58790b01b"
  )
})

testthat::test_that('live query correctly returns data.frame', {
  
  testthat::skip_on_cran()
  
  testthat::expect_true(is.data.frame(
    get_data(
      resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
      fields = c("AgeGroup", "EuropeanStandardPopulation"),
      limit = NULL,
      where = NULL,
      page_size = NULL
    )
  ))
})

testthat::test_that('correct result returned using dummy httr::RETRY result', {
  
  load(testthat::test_path("test_data", "get_data_test_2.rda"))
  
  mockery::stub(get_data, 'httr::RETRY', function(...) get_data_test_2)
  
  testthat::expect_equal(
    digest::digest(get_data(
      resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
      fields = c("AgeGroup", "EuropeanStandardPopulation"),
      limit = NULL,
      where = NULL,
      page_size = NULL
    )),
    "dd273b00e893ffab9dbe617896528060"
  )
})

testthat::test_that('correct result returned using dummy httr::RETRY result', {
  
  load(testthat::test_path("test_data", "get_data_test_3.rda"))
  
  mockery::stub(get_data, 'httr::RETRY', function(...) get_data_test_3)
  
  testthat::expect_equal(
    digest::digest(get_data(
      resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
      fields = c("AgeGroup", "EuropeanStandardPopulation"),
      limit = NULL,
      where = "\"AgeGroup\" = \'45-49 years\'",
      page_size = NULL
    )),
    "feffce07ef5b56cd1047e1640a88c835"
  )
})

testthat::test_that('using page size doesnt effect overall return', {
  
  testthat::skip_on_cran()
  
  t1 <- get_data(
    resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
    fields = c("AgeGroup", "EuropeanStandardPopulation"),
    limit = NULL,
    where = NULL,
    page_size = NULL
  )
  
  t2 <- get_data(
    resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
    fields = c("AgeGroup", "EuropeanStandardPopulation"),
    limit = NULL,
    where = NULL,
    page_size = 6
  )
  
  testthat::expect_equal(t1,t2)
})

testthat::test_that('using page size with limit doesnt effect overall return', {
  
  testthat::skip_on_cran()
  
  t1 <- get_data(
    resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
    fields = c("AgeGroup", "EuropeanStandardPopulation"),
    limit = 10,
    where = NULL,
    page_size = NULL
  )
  
  t2 <- get_data(
    resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
    fields = c("AgeGroup", "EuropeanStandardPopulation"),
    limit = 10,
    where = NULL,
    page_size = 6
  )
  
  testthat::expect_equal(t1,t2)
})


testthat::test_that('function return is same as full download', {
  
  testthat::skip_on_cran()
  
  t1 <- get_data(
    resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
    fields = c("AgeGroup", "EuropeanStandardPopulation"),
    limit = NULL,
    where = NULL,
    page_size = NULL
  )
  
  t2 <- get_resource(
    resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69"
  )[[1]]
  
  testthat::expect_equal(t1,t2)
})
