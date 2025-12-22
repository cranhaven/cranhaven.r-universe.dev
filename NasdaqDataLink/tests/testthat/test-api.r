source("test-helpers.r")

context('Test api error handling')

context('nasdaq_data_link.api')
with_mock(
  `httr::content` = function(response, as="text") {
    "{}"
  },
  `httr::VERB` = function(http, url, config, body, query) {
    httr:::response(status_code = 500)
  },
  test_that('When status code is not 200 error is thrown', {
    expect_error(nasdaq_data_link.api('datasets'))
  })
)

context('nasdaq_data_link.api.download_file')
with_mock(
  `httr::content` = function(response, as="text") {
    "{}"
  },
  `httr::GET` = function(url, config, query, ...) {
    httr:::response(status_code = 403)
  },
  test_that('When status code is not 200 error is thrown', {
    expect_error(nasdaq_data_link.api.download_file('datasets', 'foobar'))
  })
)

context('nasdaq_data_link.api.base_url')
test_that('base url is set', {
  NasdaqDataLink.api_key('test_key')
  NasdaqDataLink.base_url('https://test.this')
  path <- 'datasets'
  results <- nasdaq_data_link.api.build_request(path, start_date = '2015-01-01', end_date = as.Date('2015-02-01'))
  expected_url <- "https://test.this/api/v3/datasets"
  expect_equal(results[1], list(request_url = expected_url))
  NasdaqDataLink.base_url('https://data.nasdaq.com/')
})

context('nasdaq_data_link.api.build_request')
test_that('request headers and query params are constructed', {
  NasdaqDataLink.api_key('test_key')
  NasdaqDataLink.api_version('2015-04-09')
  path <- 'datasets'
  results <- nasdaq_data_link.api.build_request(path, start_date = '2015-01-01', end_date = as.Date('2015-02-01'))
  expected_params <- list(start_date = '2015-01-01', end_date = '2015-02-01')
  expected_headers <- list(
      Accept = 'application/json, application/vnd.nasdaq_data_link+json;version=2015-04-09',
      `Request-Source` = 'R',
      `Request-Source-Version` = '1.0.0',
      `X-Api-Token` = 'test_key'
    )
  expected_url <- "https://data.nasdaq.com/api/v3/datasets"
  expect_equal(results, list(request_url = expected_url, headers = expected_headers, params = expected_params))
})

context('nasdaq_data_link.api.build_query_params')
test_that('query params with array values are converted properly', {
  params <- list()
  params$param1 <- 'foo'
  params$param2 <- c('hello', 'world', 'bar')
  params$param3 <- 'cool'

  expected_params <- list(param1='foo', 'param2[]'='hello', 'param2[]'='world', 'param2[]'='bar', param3='cool')
  expect_equal(nasdaq_data_link.api.build_query_params(params), expected_params)
})

reset_config()
