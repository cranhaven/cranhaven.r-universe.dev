
testthat::test_that(
  "created query is as expected",{
    
    testthat::expect_equal(
      prep_nosql_query(
        resource = "42f17a3c-a4db-4965-ba68-3dffe6bca13a",
        fields = c("Dose", "Product"),
        limit = 10,
        offset = 0),
      "https://www.opendata.nhs.scot/api/3/action/datastore_search?id=42f17a3c-a4db-4965-ba68-3dffe6bca13a&fields=Dose,Product&limit=10&offset=0&sort=_id"
    )  
  })

testthat::test_that(
  "created query is as expected and works with API",{
    
    testthat::skip_on_cran()
    
    query <- prep_nosql_query(
      resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
      fields = c("AgeGroup"),
      limit = 1,
      offset = 0)
    
    res <- httr::content(httr::GET(query))
    
    testthat::expect_true(
      res$success & 
        length(res$result$records) == 1 &
        length(res$result$records[[1]]) == 1
    )
    
  })

testthat::test_that(
  "created query is as expected but fails because of
  'abc' not being a valid field name",{
    
    testthat::skip_on_cran()
    
    query <- prep_nosql_query(
      resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
      fields = c("abc"),
      limit = 1,
      offset = 0)
    
    res <- httr::content(httr::GET(query))
    
    testthat::expect_false(res$success)
    
  })

testthat::test_that(
  "query runs successfully when fields is NULL",{
    
    testthat::skip_on_cran()
    
    query <- prep_nosql_query(
      resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
      fields = NULL,
      limit = 1,
      offset=0)
    
    res <- httr::content(httr::GET(query))
    
    testthat::expect_true(res$success)
    
  })

testthat::test_that(
  "no errors when passing NULLs",{
    
    testthat::expect_equal(prep_nosql_query(
      resource = "42f17a3c-a4db-4965-ba68-3dffe6bca13a",
      fields = NULL,
      limit = NULL,
      offset = NULL),
      "https://www.opendata.nhs.scot/api/3/action/datastore_search?id=42f17a3c-a4db-4965-ba68-3dffe6bca13a&sort=_id"
      )
    
  })


testthat::test_that(
  "error for non valid resource id",{
    
    testthat::skip_on_cran()
    
    query <- prep_nosql_query(
      resource = "abc123",
      fields = NULL,
      limit = 1,
      offset = 1)
    
    res <- httr::content(httr::GET(query))
    
    testthat::expect_false(res$success)
    
  })
