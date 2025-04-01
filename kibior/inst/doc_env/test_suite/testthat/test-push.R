

context("Move data - push")


testthat::setup({
    # remove indices if they exist
    remove_all_indices()
})

testthat::teardown({
    # remove indices if they exist
    remove_all_indices()
})


# start push ----

test_that("kibior::push, wrong args", {
    expect_error(kc$push())
    # data
    expect_error(kc$push(data = 3))
    expect_error(kc$push(data = 3.123))
    expect_error(kc$push(data = "sdfff"))
    expect_error(kc$push(data = c()))
    expect_error(kc$push(data = c("w", "e", "s", "h")))
    expect_error(kc$push(data = list()))
    expect_error(kc$push(data = list("w", "e", "s", "h")))
    expect_error(kc$push(data = ds[["starwars"]]))
    # index name
    expect_error(kc$push(data = NULL, index_name = ds[["starwars"]]))
    expect_error(kc$push(index_name = ds[["starwars"]]))
    expect_error(kc$push(data = ds[["starwars"]], index_name = NA))
    expect_error(kc$push(data = ds[["starwars"]], index_name = 1))
    expect_error(kc$push(data = ds[["starwars"]], index_name = 1.234))
    expect_error(kc$push(data = ds[["starwars"]], index_name = c()))
    expect_error(kc$push( ds[["starwars"]], index_name = c("w", "e", "s", "h")))
    expect_error(kc$push( ds[["starwars"]], index_name = list()))
    expect_error(kc$push( ds[["starwars"]], index_name = list("w", "e", "s", "h")))
    # bulk 
    expect_error(kc$push( ds[["starwars"]], "sw", bulk_size = NA))
    expect_error(kc$push( ds[["starwars"]], "sw", bulk_size = TRUE))
    expect_error(kc$push( ds[["starwars"]], "sw", bulk_size = FALSE))
    expect_error(kc$push( ds[["starwars"]], "sw", bulk_size = NULL))
    expect_error(kc$push( ds[["starwars"]], "sw", bulk_size = 0))
    expect_error(kc$push( ds[["starwars"]], "sw", bulk_size = -1))
    expect_error(kc$push( ds[["starwars"]], "sw", bulk_size = -100))
    expect_error(kc$push( ds[["starwars"]], "sw", bulk_size = c()))
    expect_error(kc$push( ds[["starwars"]], "sw", bulk_size = c("w", "e", "s", "h")))
    expect_error(kc$push( ds[["starwars"]], "sw", bulk_size = list()))
    expect_error(kc$push( ds[["starwars"]], "sw", bulk_size = list("w", "e", "s", "h")))
    expect_error(kc$push( ds[["starwars"]], "sw", bulk_size = "NOPE"))
    # mode args
    a <- list(
        NA,
        TRUE,
        FALSE,
        0,
        -1,
        2.4543451,
        -100,
        c("w", "e", "s", "h"),
        list(),
        list("w", "e", "s", "h"),
        "NOPE"
    )
    # mode
    expect_error(kc$push( ds[["starwars"]], "sw", mode = NULL))
    for(i in a){
        if(length(i)<=1) expect_error(kc$push( ds[["starwars"]], "sw", mode = i))
    }
    # mode = check
    for(i in a){
        expect_error(kc$push( ds[["starwars"]], "sw", mode = "check", id_col = i))
    }
    # mode = update
    expect_error(kc$push( ds[["starwars"]], "sw", mode = "update", id_col = NULL))
    for(i in a){
        expect_error(kc$push( ds[["starwars"]], "sw", mode = "update", id_col = i))
    }
    # mode = recreate
    for(i in a){
        expect_error(kc$push( ds[["starwars"]], "sw", mode = "recreate", id_col = i))
    }
})

test_that("kibior::push, nominal case, single index, no id_col", {
    remove_all_indices()
    res <- kc$push(ds[["starwars"]], "sw")
    expect_equal(res, "sw")
    # dim ok
    expect_equal(length(kc$list()), 1)
    res <- kc$count("sw")[["sw"]]
    expect_equal(res, nrow(ds[["starwars"]]))
    res <- kc$columns("sw")[["sw"]]
    expect_setequal(res, c("kid", names(ds[["starwars"]])))
})

test_that("kibior::push, nominal case, multiple indices x data, no id_col", {
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
        res <- kc$count(d)[[d]]
        expect_equal(res, nrow(ds[[d]]))
        res <- kc$columns(d)[[d]]
        expect_setequal(res, c("kid", names(ds[[d]])))
    }
    expect_equal(length(kc$list()), length(ds))
})

test_that("kibior::push, nominal case, data with index already created, check mode", {
    remove_all_indices()
    expect_true(kc$create(single_index_name)[[single_index_name]])
    # check
    expect_error(kc$push(ds[["starwars"]], single_index_name, mode = "check"))
})

test_that("kibior::push, nominal case, data with index already created, recreate mode", {
    remove_all_indices()
    expect_true(kc$create(single_index_name)[[single_index_name]])
    # recreate
    res <- kc$push(ds[["starwars"]], single_index_name, mode = "recreate")
    expect_equal(res, single_index_name)
    expect_equal(kc$count(single_index_name)[[single_index_name]], nrow(ds[["starwars"]]))
    expect_setequal(kc$columns(single_index_name)[[single_index_name]], c("kid", names(ds[["starwars"]])))
})

test_that("kibior::push, nominal case, data with index already created, update mode", {
    remove_all_indices()
    expect_true(kc$create(single_index_name)[[single_index_name]])
    # update
    res <- kc$push(ds[["starwars"]], single_index_name, mode = "update", id_col = "name")
    expect_equal(res, single_index_name)
    expect_equal(kc$count(single_index_name)[[single_index_name]], nrow(ds[["starwars"]]))
    # x2 because no id_col, es will determine random ids
    expect_setequal(kc$columns(single_index_name)[[single_index_name]], names(ds[["starwars"]]))
})


test_that("kibior::push, nominal case, data with index not created before, check mode", {
    remove_all_indices()
    # 
    res <- kc$push(ds[["starwars"]], single_index_name, mode = "check")
    expect_equal(res, single_index_name)
    expect_equal(kc$count(single_index_name)[[single_index_name]], nrow(ds[["starwars"]]))
    expect_setequal(kc$columns(single_index_name)[[single_index_name]], c("kid", names(ds[["starwars"]])))
})

test_that("kibior::push, nominal case, data with index not created before, recreate mode", {
    remove_all_indices()
    # recreate
    res <- kc$push(ds[["starwars"]], single_index_name, mode = "recreate")
    expect_equal(res, single_index_name)
    expect_equal(kc$count(single_index_name)[[single_index_name]], nrow(ds[["starwars"]]))
    expect_setequal(kc$columns(single_index_name)[[single_index_name]], c("kid", names(ds[["starwars"]])))
})

test_that("kibior::push, nominal case, data with index not created before, update mode", {
    remove_all_indices()
    # update
    expect_error(kc$push(ds[["starwars"]], single_index_name, mode = "update"))
})


test_that("kibior::push, wrong id_col names", {
    remove_all_indices()
    # does not exists 
    expect_error(kc$push(ds[["starwars"]], single_index_name, id_col = "slkdfjslkdfjskldjf"))
    # not unique
    expect_error(kc$push(ds[["starwars"]], single_index_name, id_col = "homeworld"))
    # multiple not allowed
    expect_error(kc$push(ds[["starwars"]], single_index_name, id_col = c("homeworld", "name")))
})

test_that("kibior::push, nominal id_col, no id_col defined", {
    remove_all_indices()
    # default, let ES do
    res <- kc$push(ds[["starwars"]], single_index_name, id_col = NULL)
    expect_equal(res, single_index_name)
    expect_equal(kc$count(single_index_name)[[single_index_name]], nrow(ds[["starwars"]]))
    expect_setequal(c("kid", names(ds[["starwars"]])), kc$columns(single_index_name)[[single_index_name]])
    # Metadata "_id" ES field is a sequence
    res <- kc$pull(single_index_name, keep_metadata = TRUE)
    res_id <- res[[single_index_name]] %>% 
        dplyr::select("_id") %>% 
        as.list() %>% 
        .[["_id"]]
    res_source_id <- res[[single_index_name]] %>% 
        dplyr::select("_source.kid") %>% 
        as.list() %>% 
        .[["_source.kid"]]
    expect_setequal(res_id, res_source_id)
})

test_that("kibior::push, nominal id_col, good id_col defined", {
    remove_all_indices()
    res <- kc$push(ds[["starwars"]], single_index_name, id_col = "name") # unique col
    expect_equal(res, single_index_name)
    expect_equal(kc$count(single_index_name)[[single_index_name]], nrow(ds[["starwars"]]))
    # no "kid" col added
    expect_setequal(names(ds[["starwars"]]), kc$columns(single_index_name)[[single_index_name]])
    # Metadata "_id" ES field is what we defined
    res <- kc$pull(single_index_name, keep_metadata = TRUE)
    res_id <- res[[single_index_name]] %>% 
        dplyr::select("_id") %>% 
        as.list() %>% 
        .[["_id"]]
    res_source_id <- res[[single_index_name]] %>% 
        dplyr::select("_source.name") %>% 
        as.list() %>% 
        .[["_source.name"]]
    expect_setequal(res_id, res_source_id)
})

# end push



