library(testthat)

# Test class to test private methods in the IndustryDoscSearch class
TestDocsSearch <- R6::R6Class(
  inherit = IndustryDocsSearch,
  public = list (
    test_create_query = function(q=NULL, collection=NULL, industry=NULL, wt='json', cursorMark='*', sort='id%20asc') {
      return (private$create_query(q=q, collection=collection, industry=industry, wt=wt, cursorMark=cursorMark, sort=sort))
    },
    test_update_cursormark = function(query, cursorMark) {
      return (private$update_cursormark(query, cursorMark))
    },
    test_loop_results = function(query, n) {
      private$loop_results(query, n=n)
    },
    test_create_links = function(query, industry) {
      private$loop_results(query=query, n=100)
      private$create_links(industry)
    }
  )
)

####### Tests #######
test_that("create_query works with q parameter", {
  ids <- TestDocsSearch$new()
  query <- ids$test_create_query(
    q = 'collection:test AND industry:tobacco',
    wt = 'json',
    cursorMark = '*',
    sort = 'id%20asc'
  )

  expect_equal(
    query,
    'https://metadata.idl.ucsf.edu/solr/ltdl3/query?q=(collection:test%20AND%20industry:tobacco)&wt=json&cursorMark=*&sort=id%20asc'
  )
})

test_that("create_query works without q parameter", {
  ids <- TestDocsSearch$new()
  query <- ids$test_create_query(
    q=NULL,
    collection = 'test',
    industry = 'tobacco',
    wt = 'json',
    cursorMark = '*',
    sort = 'id%20asc'
  )

  expect_equal(
    query,
    'https://metadata.idl.ucsf.edu/solr/ltdl3/query?q=(collection:test%20AND%20industry:tobacco)&wt=json&cursorMark=*&sort=id%20asc'
  )
})

test_that("update_cursormark works correctly", {
  ids <- TestDocsSearch$new()
  query <- 'https://metadata.idl.ucsf.edu/solr/ltdl3/query?q=(collection:test)&wt=json&cursorMark=*&sort=id%20asc'
  updated_query <- ids$test_update_cursormark(query, cursorMark='123AB')

  expect_equal(
    updated_query,
    'https://metadata.idl.ucsf.edu/solr/ltdl3/query?q=(collection:test)&wt=json&cursorMark=123AB&sort=id%20asc'
  )
})

test_that("loop_results fetches correct number of results [50]", {
  ids <- TestDocsSearch$new()
  test_query <- 'https://metadata.idl.ucsf.edu/solr/ltdl3/query?q=(industry:tobacco%20AND%20case:"State%20of%20North%20Carolina"%20AND%20collection:"JUUL%20labs%20Collection"%20AND%20type:email)&wt=json&cursorMark=*&sort=id%20asc'
  ids$test_loop_results(query=test_query, n=50)
  expect_equal(
    nrow(ids$results), 50)
})

test_that("loop_results fetches correct number of results [1000]", {
  ids <- TestDocsSearch$new()
  test_query <- 'https://metadata.idl.ucsf.edu/solr/ltdl3/query?q=(industry:tobacco%20AND%20case:"State%20of%20North%20Carolina"%20AND%20collection:"JUUL%20labs%20Collection"%20AND%20type:email)&wt=json&cursorMark=*&sort=id%20asc'
  ids$test_loop_results(query=test_query, 1000)
  expect_equal(
    nrow(ids$results), 1000)
})

test_that("create_links adds correct URLs", {
  ids <- TestDocsSearch$new()
  test_query <- 'https://metadata.idl.ucsf.edu/solr/ltdl3/query?q=(industry:tobacco%20AND%20case:"State%20of%20North%20Carolina"%20AND%20collection:"JUUL%20labs%20Collection"%20AND%20type:email)&wt=json&cursorMark=*&sort=id%20asc'
  ids$test_create_links(query=test_query, industry="tobacco")
  first_url <- ids$results[[1, "url"]]
  expect_equal(first_url,'https://www.industrydocuments.ucsf.edu/tobacco/docs/#id=ffbb0284')
})

test_that("query method works with q parameter", {
  ids <- IndustryDocsSearch$new()
  ids$query(
    q = 'industry:tobacco AND case:"State of North Carolina" AND collection:"JUUL labs Collection" AND type:email',
    n = 100
  )
  expect_equal(nrow(ids$results), 100)
})

test_that("save method works with parquet", {
  ids <- IndustryDocsSearch$new()
  ids$query(
    q = 'industry:tobacco AND case:"State of North Carolina" AND collection:"JUUL labs Collection" AND type:email',
    n = 100
  )
  temp_parquet <- tempfile(fileext = ".parquet")
  expect_no_error(ids$save(temp_parquet, "parquet"))
  expect_true(file.exists(temp_parquet))
  parquet_data <- arrow::read_parquet(temp_parquet)
  expect_equal(nrow(parquet_data), 100)
  unlink(temp_parquet)
})

test_that("save method works with json", {
  ids <- IndustryDocsSearch$new()
  ids$query(
    q = 'industry:tobacco AND case:"State of North Carolina" AND collection:"JUUL labs Collection" AND type:email',
    n = 100
  )
  expect_equal(nrow(ids$results), 100)
  df_test <- ids$results
  temp_json <- tempfile(fileext = ".json")
  ids$save(temp_json, "json")
  expect_no_error(ids$save(temp_json, "json"))
  expect_true(file.exists(temp_json))
  json_data <- jsonlite::fromJSON(temp_json) %>% data.table::as.data.table()
  expect_equal(nrow(json_data), 100)
  unlink(temp_json)
})

test_that("save method works with csv", {
  ids <- IndustryDocsSearch$new()
  ids$query(
    q = 'industry:tobacco AND case:"State of North Carolina" AND collection:"JUUL labs Collection" AND type:email',
    n = 100
  )
  temp_csv <- tempfile(fileext = ".csv")
  expect_no_error(ids$save(temp_csv, "csv"))
  expect_true(file.exists(temp_csv))
  csv_data <- data.table::fread(temp_csv)
  expect_equal(nrow(csv_data), 100)
  unlink(temp_csv)
})
