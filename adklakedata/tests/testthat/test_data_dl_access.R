
context("Testing download, data, and metadata access")

test_that("test file download", {

  skip_on_cran()

  check_dl_data()

})


test_that("test data access", {
  
  skip_on_cran()
  
  for(name in names(adklakedata:::filenames)){
    expect_is(adk_data(name), 'data.frame')
  }
  
})

test_that("test metadata access", {
  
  skip_on_cran()
  
  for(name in names(adklakedata:::filenames)){
    expect_is(adk_metadata(name), 'character')
  }
  
})
