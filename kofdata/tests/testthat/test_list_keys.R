context("list_keys_in_collection")

test_that("list_keys_in_collection works", {

  sets <- list_available_collections()

  if(nrow(sets) > 0) {
    keys <- list_keys_in_collection(sets[1, "collection_name"])
    expect_true(length(keys) > 0)
    expect_is(keys, "character")
  }

  if(nrow(sets) > 1) {
    keylists <- list_keys_in_collection(sets[1:2, "collection_name"])
    expect_is(keylists, "list")
    expect_equal(names(keylists), sets[1:2, "collection_name"])
  }
})

test_that("list_keys_in_collection works for private collection", {
  apikey <- Sys.getenv("KOFDATA_TEST_SECRET")
  skip_if(nchar(apikey) == 0)

  keys <- list_keys_in_collection("kofdata_test_collection", apikey)

  expect_equal(keys, c("ch.kof.barometer", "ch.kof.globalbaro.leading"))
})
