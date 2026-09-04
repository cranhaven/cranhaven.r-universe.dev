context("list_available_collections")

test_that("list_available_collections works", {
  collections <- list_available_collections()

  expect_is(collections, "data.frame")
  expect_equal(ncol(collections), 3)
  expect_equal(names(collections), c("collection_name", "collection_description", "is_public"))

  if(nrow(collections) > 0) {
    expect_is(collections[1, "collection_name"], "character")
    expect_is(collections[1, "collection_description"], "character")
    expect_is(collections[1, "is_public"], "logical")
    expect_true(all(collections[, "is_public"]))
  }
})

test_that("list_available_collections lists users collections", {
  apikey <- Sys.getenv("KOFDATA_TEST_SECRET")
  skip_if(nchar(apikey) == 0)

  collections <- list_available_collections(apikey)

  testcoll <- collections[collections$collection_name == "kofdata_test_collection", ]
  expect_equal(nrow(testcoll), 1)
  expect_false(testcoll$is_public)
})
