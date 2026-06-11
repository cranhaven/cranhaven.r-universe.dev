context("test utils")


test_that(".get_cumulocity_* does not result in error", {
  skip_on_cran()
  expect_error(.get_cumulocity_base_url(), NA)
  expect_error(.get_cumulocity_usr(), NA)
  expect_error(.get_cumulocity_pwd(), NA)
  expect_error(.get_cumulocity_device_id(), NA)
})


test_that(".get_env throws error if var is not found", {
  expect_error(.get_env("dummy_var_00"), "env var 'dummy_var_00' not found.")
})


test_that(".check_date throws error if input is not of character class", {
  expect_error(.check_date(123), "Dates must be of class character.")

  expect_error(
    .check_date(strptime("2019-08-28T18:53:15Z",
      format = "%Y-%m-%dT%H:%M:%OSZ", tz = "Z"
    )),
    "Dates must be of class character."
  )

  expect_error(
    .check_date(as.POSIXlt("2000-08-28T18:53:15Z")),
    "Dates must be of class character."
  )
})

# test_that(".get_devices does not throw error", {
#   skip_on_cran()
#   expect_error(.get_devices(page_size = 3), NA)
#   expect_equal(.get_devices(page_size = 20)$status_code, 200)
# })


test_that(".check_response_for_error returns error for bad credentials", {
  skip_on_cran()
  exp_error <- paste("Cumulocity API request failed with status code 401.\n",
    "Category:         Client error",
    "Reason:           Unauthorized",
    "Status message:   Client error: (401) Unauthorized",
    "Response message: Invalid credentials! : Bad credentials",
    sep = "\n"
  )

  url_01 <- paste0(.get_cumulocity_base_url(),
    "/inventory/managedObjects?fragmentType=c8y_IsDevice",
    collapse = ""
  )

  result_03 <- httr::GET(url = url_01, httr::authenticate("foo", "bar"))
  cont_03 <- httr::content(result_03, "text")
  cont_parsed_03 <- jsonlite::fromJSON(cont_03)

  expect_error(.check_response_for_error(result_03, cont_parsed_03), exp_error, fixed = TRUE)
})


test_that(".check_response_for_error uses $error if $message is null", {
  skip_on_cran()

  url_01 <- paste0(.get_cumulocity_base_url(),
    "/inventory/managedObjects?fragmentType=c8y_IsDevice",
    collapse = ""
  )

  result_03 <- httr::GET(url = url_01, httr::authenticate("foo", "bar"))
  cont_03 <- httr::content(result_03, "text")
  cont_parsed_03 <- jsonlite::fromJSON(cont_03)

  cont_parsed_03_null_message <- cont_parsed_03
  cont_parsed_03_null_message$message <- NULL

  exp_error <- paste("Cumulocity API request failed with status code 401.\n",
    "Category:         Client error",
    "Reason:           Unauthorized",
    "Status message:   Client error: (401) Unauthorized",
    "Response message: security/Unauthorized",
    sep = "\n"
  )
  expect_error(.check_response_for_error(result_03, cont_parsed_03_null_message), exp_error, fixed = TRUE)
})


test_that(".check_response_for_error returns blank for
          Response message if $message and $error are null", {
  skip_on_cran()

  url_01 <- paste0(.get_cumulocity_base_url(),
    "/inventory/managedObjects?fragmentType=c8y_IsDevice",
    collapse = ""
  )

  result_03 <- httr::GET(url = url_01, httr::authenticate("foo", "bar"))
  cont_03 <- httr::content(result_03, "text")
  cont_parsed_03 <- jsonlite::fromJSON(cont_03)

  cont_parsed_03_both_null <- cont_parsed_03
  cont_parsed_03_both_null$message <- NULL
  cont_parsed_03_both_null$error <- NULL

  exp_error <- paste("Cumulocity API request failed with status code 401.\n",
    "Category:         Client error",
    "Reason:           Unauthorized",
    "Status message:   Client error: (401) Unauthorized",
    "Response message: ",
    sep = "\n"
  )
  expect_error(.check_response_for_error(result_03, cont_parsed_03_both_null), exp_error, fixed = TRUE)
})


# test_that(".parse_datetime returns POSIXlt object", {
#   expect_true(inherits(.parse_datetime("2119-09-10T13:06:34.161Z"), "POSIXlt"))
# })



# test_that("query is formed correctly", {
#   query_01 <- .form_query(
#     device_id = 111,
#     date_from = "1980-03-11T13:22:43Z",
#     date_to = "1990-02-23T14:33:34Z",
#     page_size = 99
#   )
#
#   expect_equal(query_01$source, 111)
#   expect_equal(query_01$dateFrom, "1980-03-11T13:22:43Z")
#   expect_equal(query_01$dateTo, "1990-02-23T14:33:34Z")
#   expect_null(query_01$pageSize)
#
#   query_02 <- .form_query(
#     device_id = 111,
#     date_from = NULL,
#     date_to = "1990-02-23T14:33:14Z",
#     page_size = 99
#   )
#
#   expect_equal(query_02$source, 111)
#   expect_null(query_02$dateFrom)
#   expect_equal(query_02$dateTo, "1990-02-23T14:33:14Z")
#   expect_equal(query_02$pageSize, 99)
#
#   query_03 <- .form_query(
#     device_id = 111,
#     date_from = NULL,
#     date_to = NULL,
#     page_size = 99
#   )
#
#   expect_equal(query_03$source, 111)
#   expect_null(query_03$dateFrom)
#   expect_null(query_03$dateFrom)
#   expect_equal(query_03$pageSize, 99)
#
#   query_04 <- .form_query(
#     device_id = 111,
#     date_from = "1980-03-11T13:22:44Z",
#     date_to = NULL,
#     page_size = 99
#   )
#
#   expect_equal(query_04$source, 111)
#   expect_equal(query_04$dateFrom, "1980-03-11T13:22:44Z")
#   expect_null(query_04$dateTo)
#   expect_equal(query_04$pageSize, 99)
# })
