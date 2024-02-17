
testthat::test_that('function correctly return list of data sets', {
  
  testthat::skip_on_cran()
  
  testthat::expect_true(is.list(
    get_resource(package = "standard-populations", limit = 1L)
  ))
})

testthat::test_that('function correctly return list of data sets', {
  
  testthat::skip_on_cran()
  
  testthat::expect_true(is.list(
    get_resource(package = "standard-populations",
                resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
                limit = 1L
    )
  ))
})

testthat::test_that('check all returns have had row limit applied', {
  
  testthat::skip_on_cran()
  
  res <- get_resource(package = "standard-populations",
                     limit = 5L
  )
  
  testthat::expect_true(all(unlist(lapply(res, nrow)) == 5))
})

testthat::test_that("error when no resource or package specified", {
  testthat::expect_error(get_resource())
})

testthat::test_that('one resource is returned using ids', {
  
  load(testthat::test_path("test_data", "res_all_res.rda"))
  
  mockery::stub(get_resource, 'all_resources', function(...) res_all_res)
  
  testthat::expect_equal(digest::digest(get_resource(
    package = "4dd86111-7326-48c4-8763-8cc4aa190c3e",
    resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
    limit = 5L
  )),
  "6ae498a60e58339c38aa0afaf2aeb10e"
  )
})

testthat::test_that('one resource is returned using names', {
  
  load(testthat::test_path("test_data", "res_all_res.rda"))
  
  mockery::stub(get_resource, 'all_resources', function(...) res_all_res)
  
  testthat::expect_equal(digest::digest(get_resource(
    package = "standard-populations",
    resource = "European Standard Population",
    limit = 5L
  )),
  "6ae498a60e58339c38aa0afaf2aeb10e"
  )
})


testthat::test_that('correct return with one not found package name and message raised', {
  
  load(testthat::test_path("test_data", "res_all_res.rda"))
  
  mockery::stub(get_resource, 'all_resources', function(...) res_all_res)
  
  suppressMessages({
  testthat::expect_message(get_resource(
    package = c("standard-populations", "nonsense"),
    resource = "European Standard Population",
    limit = 5L
  ))
  
  testthat::expect_equal(digest::digest(get_resource(
    package = c("standard-populations", "nonsense"),
    resource = "European Standard Population",
    limit = 5L
  )),
  "6ae498a60e58339c38aa0afaf2aeb10e"
  )
  })
})


testthat::test_that('correct return with one not found resource name and message raised', {
  
  load(testthat::test_path("test_data", "res_all_res.rda"))
  
  mockery::stub(get_resource, 'all_resources', function(...) res_all_res)
  
  suppressMessages({
  testthat::expect_message(get_resource(
    package = "standard-populations",
    resource = c("European Standard Population", "nonsense"),
    limit = 5L
  ))
  
  testthat::expect_equal(digest::digest(get_resource(
    package = "standard-populations",
    resource = c("European Standard Population", "nonsense"),
    limit = 5L
  )),
  "6ae498a60e58339c38aa0afaf2aeb10e"
  )
  })
})

testthat::test_that('correct return with one not found resource name and message 
                    raised, specifically testing valid resource name but not
                    within the package specified', {
  
  load(testthat::test_path("test_data", "res_all_res.rda"))
  
  mockery::stub(get_resource, 'all_resources', function(...) res_all_res)
  
  suppressMessages({
  testthat::expect_message(get_resource(
    package = c("standard-populations", "nonsense"),
    resource = "European Standard Population",
    limit = 5L
  ))
  
    testthat::expect_equal(digest::digest(get_resource(
    package = "standard-populations",
    resource = c("European Standard Population", "Settings and Events"),
    limit = 5L
  )),
  "6ae498a60e58339c38aa0afaf2aeb10e"
    )
  })
  })

testthat::test_that('specifying only a resource works', {
  
  load(testthat::test_path("test_data", "res_all_res.rda"))
  
  mockery::stub(get_resource, 'all_resources', function(...) res_all_res)
  
  testthat::expect_equal(digest::digest(get_resource(
    resource = c("European Standard Population"),
    limit = 5L
  )),
  "6ae498a60e58339c38aa0afaf2aeb10e"
  )
})


testthat::test_that('empty list when package not found', {
  
  testthat::skip_on_cran()
  
  testthat::expect_equal(
    suppressWarnings(get_resource(package = "a_nonsense_value", limit = 1L)),
    list()
  )
})
