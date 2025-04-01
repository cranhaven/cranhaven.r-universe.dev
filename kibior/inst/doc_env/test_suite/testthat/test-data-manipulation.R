

context("Data manipulation")


testthat::setup({
    # remove indices if they exist
    remove_all_indices()
})

testthat::teardown({
    # remove indices if they exist
    remove_all_indices()
})


# index_name, column, max_size = 1000

# start keys ----

test_that("kibior::keys, no args", {
    expect_error(kc$keys())
})

test_that("kibior::keys, no col", {
    remove_all_indices()
    # no index
    expect_error(kc$keys("what"))
    # right index
    ds[["starwars"]] %>% kc$push("sw")
    expect_error(kc$keys("sw"))
})

test_that("kibior::keys, wrong col", {
    remove_all_indices()
    ds[["starwars"]] %>% kc$push("sw")
    #
    expect_error(kc$keys("sw", "nope"))
    expect_error(kc$keys("sw", c("nope", "nopenope")))
})

test_that("kibior::keys, nominal case", {
    remove_all_indices()
    ds[["starwars"]] %>% kc$push("sw")
    #
    expected_names <- ds[["starwars"]] %>% 
        dplyr::select(name) %>% 
        .$name %>% 
        unique()
    expect_setequal(kc$keys("sw", "name"), expected_names)
})


# end keys ----