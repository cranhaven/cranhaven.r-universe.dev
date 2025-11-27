test_that("setting cookies works", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("webfakes")
  expect_equal({
    domain <- httr2::url_parse(httr2::example_url())$hostname
    add_cookies(cookiestring = "snicker=doodle; password=secret", domain = domain)
    httr2::request(example_url()) |>
      httr2::req_url_path("/cookies/set") |>
      req_cookiemonster_set() |>
      httr2::req_perform() |>
      httr2::resp_body_json()
  }, list(cookies = list(snicker = "doodle", password = "secret")))
})

test_that("only works with reqs", {
  skip_if_not_installed("httr2")
  expect_error(req_cookiemonster_set(list()), "an.HTTP.request.object")
  expect_error(req_cookiemonster_set(""), "an.HTTP.request.object")
})
