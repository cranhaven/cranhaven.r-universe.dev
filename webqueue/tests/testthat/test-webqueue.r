test_that("webqueue", {
  
  # library(testthat); library(webqueue)
  
  withr::local_envvar(CURL_SSL_BACKEND = NA)
  
  
  expect_error(WebQueue$new(handler = NULL, parse = ~{ NULL }, onHeaders = ~{ NULL }))
  expect_error(WebQueue$new(handler = ~{ NULL }, globals = NULL))
  expect_error(WebQueue$new(handler = ~{ NULL }, globals = list('.wq_handler' = 1)))
  expect_error(WebQueue$new(handler = ~{ NULL }, parse = NA))
  expect_error(WebQueue$new(handler = ~{ NULL }, init = { Sys.sleep(10) }, timeout = c(starting = 1)))
  
  
  
  fetch <- function (cookies = NULL, post = NULL, query = NULL, path = '') {
    
    req <- httr2::request(paste0('http://127.0.0.1:8080', path))
    req <- httr2::req_timeout(req, 5L)
    req <- httr2::req_error(req, is_error = function (resp) FALSE)
    
    if (!is.null(cookies)) req <- httr2::req_headers(req, "Cookie" = cookies)
    if (!is.null(post))    req <- httr2::req_body_json(req, post)
    if (!is.null(query))   req <- httr2::req_url_query(req, !!!query)
    
    resp <- httr2::req_perform(req)
    resp <- list(
      body   = httr2::resp_body_string(resp),
      status = httr2::resp_status(resp) )
    
    return (resp)
  }
  
  handler <- function (request) {
    if (!is.null(x <- request$COOKIES$txt)) return (x)
    if (!is.null(x <- request$ARGS$txt))    return (x)
    return ('Hello World!')
  }
  
  parse <- function (request) {
    error_class <- request$ARGS$err
    if (is.null(error_class)) return (request) 
    stop(errorCondition('err', class = error_class))
  }
  
  
  # Foreground process
  
  wq  <- WebQueue$new(
    handler = handler, 
    parse   = parse, 
    workers = 1L, 
    bg      = FALSE )
  
  expect_s3_class(wq, class = c('WebQueue', 'R6'))
  expect_identical(wq$url, 'http://127.0.0.1:8080')
  expect_no_error(suppressMessages(wq$print()))
  
  worker <- jobqueue::Worker$new(globals = list(fetch = fetch))
  worker$wait()
  
  job <- jobqueue::Job$new({ fetch() })
  worker$run(job)
  expect_identical(job$result$body, 'Hello World!')
  
  job <- jobqueue::Job$new({ fetch(cookies = 'xyz; b=5; txt=1=6') })
  worker$run(job)
  expect_identical(job$result$body, '1=6')
  
  job <- jobqueue::Job$new({ fetch(query = list(txt='ABC')) })
  worker$run(job)
  expect_identical(job$result$body, 'ABC')
  
  job <- jobqueue::Job$new({ fetch(post = list(txt='XYZ')) })
  worker$run(job)
  expect_identical(job$result$body, 'XYZ')
  
  job <- jobqueue::Job$new({ fetch(post = list(err='timeout')) })
  worker$run(job)
  expect_identical(job$result$status, 408L)
  
  job <- jobqueue::Job$new({ fetch(post = list(err='superseded')) })
  worker$run(job)
  expect_identical(job$result$status, 409L)
  
  job <- jobqueue::Job$new({ fetch(post = list(err='interrupt')) })
  worker$run(job)
  expect_identical(job$result$status, 499L)
  
  worker$stop()
  expect_silent(wq$stop())
  
  
  
  # Background process.
  
  tmp <- tempfile()
  wq  <- WebQueue$new(
    handler     = handler, 
    parse       = parse, 
    workers     = 1L, 
    staticPaths = c('/tmp' = tmp) )
  
  expect_identical(wq$url, 'http://127.0.0.1:8080')
  
  expect_identical(fetch()$body, 'Hello World!')
  expect_identical(fetch(cookies = 'xyz; b=5; txt=1=6')$body, '1=6')
  expect_identical(fetch(query   = list(txt='ABC'))$body, 'ABC')
  expect_identical(fetch(post    = list(txt='XYZ'))$body, 'XYZ')
  expect_identical(fetch(post    = list(err='timeout'))$status,    408L)
  expect_identical(fetch(post    = list(err='superseded'))$status, 409L)
  expect_identical(fetch(post    = list(err='interrupt'))$status,  499L)
  
  cat(file = file.path(tmp, 'static.txt'), 'Static Content')
  expect_identical(fetch(path = '/tmp/static.txt')$body, 'Static Content')
  
  expect_no_error(wq$stop())
  
  
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  
  expect_identical(format_200(I(200L)), I(200L))
  expect_identical(format_500(I(500L)), I(500L))
  
  expect_identical(format_500(500L)$status, 500L)
  expect_identical(format_500(errorCondition('x'))$status, 500L)
  
  err_ti <- errorCondition('x', class='timeout')
  err_su <- errorCondition('x', class='superseded')
  err_in <- errorCondition('x', class='interrupt')
  
  expect_identical(format_500(err_ti)$status, 408L)
  expect_identical(format_500(err_su)$status, 409L)
  expect_identical(format_500(err_in)$status, 499L)
  
  expect_identical(format_500(rlang::error_cnd(parent = err_ti))$status, 408L)
  expect_identical(format_500(rlang::error_cnd(parent = err_su))$status, 409L)
  expect_identical(format_500(rlang::error_cnd(parent = err_in))$status, 499L)
  
  
  skip_on_covr()
  
  expect_error(WebQueue$new(handler = ~{ NULL }, init = { stop('blah') }))
  expect_error(WebQueue$new(handler = ~{ NULL }, init = { q(save = 'no') }))
})
