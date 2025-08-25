test_that("getInfo awaits pending result and retrieves command info", {
  result <- methods::new("ArmadilloResult",
    conn = connection,
    rval = list(result = NULL, async = TRUE)
  )

  get <- mock(list(status_code = 200))
  last_command <- list(
    id = "32acb9d0-3a9f-47b6-80e5-475966d9d23a",
    status = "COMPLETED",
    expression = "try(base::serialize({base::colnames(D)}, NULL))",
    startDate = "2020-05-06T20:59:36.253850Z",
    endDate = "2020-05-06T20:59:36.297290Z",
    withResult = TRUE,
    createDate = "2020-05-06T20:59:36.219077Z"
  )
  content <- mock(last_command)
  info <- with_mock(
    "DSMolgenisArmadillo:::.retry_until_last_result" = mock(NULL),
    "httr::GET" = get,
    "httr::content" = content,
    dsGetInfo(result)
  )

  expect_equal(info, last_command)
  expect_args(get, 1,
    handle = connection@handle,
    path = "/lastcommand",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("getInfo returns status completed for synchronous result", {
  result <- methods::new("ArmadilloResult",
    conn = connection,
    rval = list(result = "Hello", async = FALSE)
  )
  expect_equal(dsGetInfo(result), list(status = "COMPLETED"))
})

test_that("dsFetch retrieves last result for pending result", {
  result <- methods::new("ArmadilloResult",
    conn = connection,
    rval = list(result = NULL, async = TRUE)
  )

  retry <- mock(list(status_code = 200))
  content <- mock(base::serialize("Hello World!", NULL))
  value <- with_mock(
    "httr::RETRY" = retry,
    "httr::content" = content,
    dsFetch(result)
  )

  expect_equal(value, "Hello World!")
  expect_args(retry, 1,
    verb = "GET",
    handle = connection@handle,
    path = "/lastresult",
    terminate_on = c(200, 404, 401),
    httr::add_headers(c("Accept" = "application/octet-stream",
                        "Authorization" = "Bearer token"))
  )
})

test_that("dsIsCompleted retrieves status of COMPLETED async command", {
  result <- methods::new("ArmadilloResult",
    conn = connection,
    rval = list(result = NULL, async = TRUE)
  )

  content <- mock(list(status = "COMPLETED"))
  get <- mock(content)

  value <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    dsIsCompleted(result)
  )

  expect_equal(value, TRUE)
  expect_args(get, 1,
    handle = connection@handle,
    path = "/lastcommand",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsIsCompleted retrieves status of FAILED async command", {
  result <- methods::new("ArmadilloResult",
    conn = connection,
    rval = list(result = NULL, async = TRUE)
  )

  content <- mock(list(status = "FAILED"))
  get <- mock(content)

  value <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    dsIsCompleted(result)
  )

  expect_equal(value, TRUE)
  expect_args(get, 1,
    handle = connection@handle,
    path = "/lastcommand",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsIsCompleted retrieves status of RUNNING async command", {
  result <- methods::new("ArmadilloResult",
    conn = connection,
    rval = list(result = NULL, async = TRUE)
  )

  content <- mock(list(status = "RUNNING"))
  get <- mock(content)

  value <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    dsIsCompleted(result)
  )

  expect_equal(value, FALSE)
  expect_args(get, 1,
    handle = connection@handle,
    path = "/lastcommand",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsIsCompleted returns status of sync command", {
  result <- methods::new("ArmadilloResult",
    conn = connection,
    rval = list(result = NULL, async = FALSE)
  )

  value <- dsIsCompleted(result)

  expect_equal(value, TRUE)
})

test_that("getInfo returns correct list for 404 errors", {
  result <- methods::new("ArmadilloResult",
                         conn = connection,
                         rval = list(result = NULL, async = TRUE)
  )

  get <- mock(list(status_code = 404))
  info <- with_mock(
    "DSMolgenisArmadillo:::.retry_until_last_result" = mock(NULL),
    "httr::GET" = get,
    dsGetInfo(result)
  )
  expect_equal(
    info,
    list(
      status = "FAILED",
      error = paste0("No value table exists with the specified name.")
  ))
})
