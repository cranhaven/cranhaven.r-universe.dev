

context("Move data - pull - p1")


testthat::setup({
    # remove indices if they exist
    remove_all_indices()
})

testthat::teardown({
    # remove indices if they exist
    remove_all_indices()
})







# start pull ----

test_that("kibior::pull, wrong args", {
    remove_all_indices()
    res <- kc$push(ds[["starwars"]], single_index_name)
    expect_equal(res, single_index_name)
    # args to test
    a = list(
        NA, 
        TRUE, 
        FALSE, 
        0, 
        -1, 
        -100,
        NULL, 
        c(),
        c("w", "e", "s", "h"), 
        c("name", "nope"),
        list(), 
        list("w", "e", "s", "h"), 
        "NOPE" 
    )
    # no args
    expect_error(kc$pull())
    # index name
    expect_error(kc$pull(NA))
    expect_error(kc$pull(TRUE))
    expect_error(kc$pull(FALSE))
    expect_error(kc$pull(0))
    expect_error(kc$pull(-1))
    expect_error(kc$pull(-10))
    expect_error(kc$pull(list()))
    expect_error(kc$pull(list("w", "e", "s", "h")))
    # bulk 
    for(i in a){
        expect_error(kc$pull(single_index_name, bulk_size = NA))
    }
    # max size
    for(i in a){
        if(!purrr::is_null(i)){ 
            # if max_size is not null, else it returns everything
            expect_error(kc$pull(single_index_name, max_size = i))
        }
    }
    # scroll timer
    for(i in a){
        expect_error(kc$pull(single_index_name, scroll_timer = i))
    }
    # keep metadata 
    for(i in a){
        if(!is.logical(i)){
            expect_error(kc$pull(single_index_name, keep_metadata = i))
        }
    }
    expect_error(kc$pull(single_index_name, keep_metadata = NA))
    # columns filters 
    for(i in a){
        # if columns is not null or string, else it returns everything
        if(!purrr::is_null(i) && !is.character(i)){ 
            
            expect_error(kc$pull(single_index_name, columns = i))
        }
    }
    # columns filters with metadata
    for(i in a){
        # if columns is not null or string, else it returns everything
        if(!purrr::is_null(i) && !is.character(i)){ 
            expect_error(kc$pull(single_index_name, keep_metadata = TRUE, columns = i))
        }
    }
    # query
    for(i in a){
        if(is.character(i) && !("NOPE" %in% i)){ # this works but returns no results
            expect_error(kc$pull(single_index_name, query = i))
        }
    }
})



test_that("kibior::pull, nominal simple case, get one index", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # 
    for(d in names(ds)){
        r <- kc$pull(d)[[d]]
        expect_setequal(names(r), c("kid", kc$columns(d)[[d]]))
        expect_equal(nrow(r), kc$count(d)[[d]])
    }
})

test_that("kibior::pull, nominal simple case, get two indices", {
    # push data
    remove_all_indices()
    selected_columns <- list()
    selected_columns_with_source <- list()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # get "starwars" and "storms" datasets
    r <- kc$pull(c("starwars", "storms"))
    expect_length(r, 2)
    expect_setequal(c("starwars", "storms"), names(r))
    # test names
    expect_setequal(c("kid", names(ds[["starwars"]])), names(r$starwars))
    expect_setequal(c("kid", names(dplyr::storms)), names(r$storms))
    # test dimensions
    expect_equal(nrow(ds[["starwars"]]), nrow(r$starwars))
    expect_equal(nrow(dplyr::storms), nrow(r$storms))
})

test_that("kibior::pull, nominal simple case, get indices via pattern", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # get "starwars" and "storms" datasets
    r <- kc$pull("s*")
    expect_length(r, 2)
    expect_setequal(c("starwars", "storms"), names(r))
    # test names
    expect_setequal(c("kid", names(ds[["starwars"]])), names(r$starwars))
    expect_setequal(c("kid", names(dplyr::storms)), names(r$storms))
    # test dimensions
    expect_equal(nrow(ds[["starwars"]]), nrow(r$starwars))
    expect_equal(nrow(dplyr::storms), nrow(r$storms))
})


test_that("kibior::pull, wrong index names", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        ds[[d]] %>% 
            kc$push(d) %>% 
            expect_equal(d)
    }
    expect_setequal(kc$list(), names(ds))
    #
    expect_null(kc$pull("aaaa"))
})


# end pull



