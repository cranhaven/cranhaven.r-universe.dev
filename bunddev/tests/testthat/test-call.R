test_that("bunddev_call performs a request", {
  skip_if_offline()
  skip_on_cran()

  endpoints <- bunddev_endpoints("abfallnavi")
  candidates <- dplyr::filter(
    endpoints,
    method == "get",
    !stringr::str_detect(path, "\\{")
  )

  if (nrow(candidates) == 0) {
    skip("No suitable endpoint without path params.")
  }

  operation_id <- candidates$operation_id[[1]]
  response <- bunddev_call("abfallnavi", operation_id, parse = "text")
  expect_type(response, "character")
  expect_true(nchar(response) > 0)
})

test_that("cache keys are unique for different path parameters", {
  # Test that path parameters are included in cache key generation
  key1 <- bunddev:::bunddev_response_cache_key(
    "autobahn",
    "list-roadworks",
    list(roadId = "A1")
  )

  key2 <- bunddev:::bunddev_response_cache_key(
    "autobahn",
    "list-roadworks",
    list(roadId = "A3")
  )

  key3 <- bunddev:::bunddev_response_cache_key(
    "autobahn",
    "list-roadworks",
    list(roadId = "A1")
  )

  # Different path parameters should have different cache keys
  expect_false(unname(key1) == unname(key2))

  # Same path parameters should have the same cache key
  expect_equal(unname(key1), unname(key3))
})
