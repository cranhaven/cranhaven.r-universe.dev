test_that(".handle_last_command_error throws error message", {
  ok <- list(status_code = 200)
  get <- mock(ok)
  json_returned <- mock(list(status = "FAILED", message = "Error", expression = "value={broken command"))
  json_formatted <- mock(list(message = "informative error message"))

  setClass("connection", slots = list(handle = "character",
                                      token = "character",
                                      name = "character"))
  connection <- new("connection", handle = "test", token = "token", name = "test_cohort")

  expect_error(
    with_mock(
      .handle_last_command_error(connection),
      "httr::GET" = get,
      "httr::content" = json_returned,
      "jsonlite::fromJSON" = json_formatted
    ), "Error"
  )
  expect_args(get, 1, handle = connection@handle, path = "/lastcommand",
              config = httr::add_headers(c("Authorization" =
                                             paste0("Bearer ",
                                                    connection@token))))
  expect_args(json_returned, 1, ok)
})

test_that(".handle_last_command_error only works if status is FAILED", {
  setClass("connection", slots = list(handle = "character",
                                      token = "character"))
  connection <- new("connection", handle = "test", token = "token")
  ok <- list(status_code = 200)
  get <- mock(ok)
  content <- mock(list(status = "COMPLETED"))

  with_mock(
    .handle_last_command_error(connection),
    "httr::GET" = get,
    "httr::content" = content
  )

  expect_args(get, 1, handle = connection@handle, path = "/lastcommand",
              config = httr::add_headers(c("Authorization" =
                                             paste0("Bearer ",
                                                    connection@token))))
})

test_that(".handle_request_error handles 401", {
  expect_error(
    .handle_request_error(list(status_code = 401)),
    "Unauthorized"
  )
})

test_that(".handle_request_error handles 400", {
  response <- list(status_code = 400)
  httr_content <- mock(list(message = "Error"))
  with_mock(
    expect_error(
      .handle_request_error(response),
      "Bad request: Error"
    ),
    "httr::content" = httr_content
  )
})

test_that(".handle_request_error handles 500", {
  response <- list(status_code = 500)
  httr_content <- mock(message = "Something went wrong while reading/writing in the storage")
  with_mock(
    expect_error(
      .handle_request_error(response),
      "Internal server error: Something went wrong while reading/writing in the storage",
    ),
    "httr::content" = httr_content
  )
})

test_that(".unlist_character_list handles empty list", {
  expect_equal(.unlist_character_list(list()), character())
})

test_that(".unlist_character_list unnests list", {
  input <- list(foo = list(a = "a", b = list(c = "d")))
  expect_equal(
    .unlist_character_list(input),
    c("foo.a" = "a", "foo.b.c" = "d")
  )
})

test_that(".deparse deparses vectors", {
  skip("I still don't get the .deparse arguments")
})

test_that(".retry_until_last_result handles 404 by retrieving lastcommand", {
  not_found <- list(status_code = 404)
  ok <- list(status_code = 200)
  httr_retry <- mock(not_found)
  httr_get <- mock(ok)
  json_returned <- mock(list(status = "FAILED", message = "Error", expression = "value={broken command"))
  json_formatted <- mock(list(message = "informative error message"))

  setClass("connection", slots = list(handle = "character",
                                      token = "character",
                                      name = "character"))
  connection <- new("connection", handle = "test", token = "token", name = "test_cohort")

  expect_error(
    with_mock(
      .retry_until_last_result(connection),
      "httr::GET" = httr_get,
      "httr::content" = json_returned,
      "httr::RETRY" = httr_retry,
      "jsonlite::fromJSON" = json_formatted
    ), "Command 'broken command' failed on test_cohort: Error whilst evaluating"
  )

  expect_args(httr_get, 1, handle = connection@handle, path = "/lastcommand",
              config = httr::add_headers(c("Authorization" =
                                             paste0("Bearer ",
                                                    connection@token))))
  expect_args(json_returned, 1, ok)
})

test_that(".retry_until_last_result handles 404 by retrieving lastcommand", {
  not_found <- list(status_code = 404)
  ok <- list(status_code = 200)
  httr_retry <- mock(not_found)
  httr_get <- mock(ok)
  json_returned <- mock(list(status = "FAILED", message = "Error", expression = "value={broken command"))
  json_formatted <- mock(list(message = "informative error message"))

  setClass("connection", slots = list(handle = "character",
                                      token = "character",
                                      name = "character"))
  connection <- new("connection", handle = "test", token = "token", name = "test_cohort")

  expect_error(
    with_mock(
      .retry_until_last_result(connection),
      "httr::GET" = httr_get,
      "httr::content" = json_returned,
      "httr::RETRY" = httr_retry,
      "jsonlite::fromJSON" = json_formatted
    ), "Command 'broken command' failed on test_cohort: Error whilst evaluating"
  )

  expect_args(httr_get, 1, handle = connection@handle, path = "/lastcommand",
              config = httr::add_headers(c("Authorization" =
                                             paste0("Bearer ",
                                                    connection@token))))
  expect_args(json_returned, 1, ok)
})

test_that(".handle_last_command_error handles 404 error", {
  get <- mock(list(status = 404))
  json_returned <- mock(list(status = "FAILED", message = "Error", expression = "value={broken command"))
  json_formatted <- mock(list(message = "informative error message"))

  setClass("connection", slots = list(handle = "character",
                                      token = "character",
                                      name = "character"))
  connection <- new("connection", handle = "test", token = "token", name = "test_cohort")

  expect_silent(
    with_mock(
      .handle_last_command_error(connection),
      "httr::GET" = get,
      "httr::content" = json_returned,
      "jsonlite::fromJSON" = json_formatted
    )
  )
})
