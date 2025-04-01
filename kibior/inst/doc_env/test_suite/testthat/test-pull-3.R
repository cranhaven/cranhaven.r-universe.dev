

context("Move data - pull - p3")


testthat::setup({
    # remove indices if they exist
    remove_all_indices()
})

testthat::teardown({
    # remove indices if they exist
    remove_all_indices()
})







# start pull ----



test_that("kibior::pull, keep metadata, single index", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    for(d in names(ds)){
        # ask meta
        r <- kc$pull(d, keep_metadata = TRUE)[[d]]
        expect_equal(r[["_index"]][[1]], d)
        expect_setequal(r[["_id"]], r[["_source.kid"]])
        # compare colnames with no metadata result
        rr <- kc$pull(d, keep_metadata = FALSE)[[d]]
        expect_equal(nrow(rr), nrow(r))
        colnames <- names(r) %>% 
        lapply(function(x){ if(startsWith(x, "_source.")) x else NULL }) %>% 
        lapply(function(x){ gsub("_source.", "", x) }) %>% 
        unlist(use.names = FALSE)
        expect_setequal(colnames, names(rr))
    }
})

test_that("kibior::pull, keep metadata, multiple indices", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # ask meta
    r <- kc$pull(c("starwars", "storms"), keep_metadata = TRUE)
    expect_length(r, 2)
    # dimensions
    for(i in names(r)){
        # test some metadata cols
        expect_true(all(c("_index", "_version") %in% names(r[[i]])))
        # select cols with "_source." in the name and remove the rest
        colnames <- names(r[[i]]) %>% 
        lapply(function(x){ if(startsWith(x, "_source.")) x else NULL }) %>% 
        lapply(function(x){ gsub("_source.", "", x) }) %>% 
        unlist(use.names = FALSE)
        expect_true(all(names(ds[[i]]) %in% colnames))
    }
})

test_that("kibior::pull, keep metadata, indices via pattern", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # ask meta
    r <- kc$pull("s*", keep_metadata = TRUE)
    expect_length(r, 2)
    # dimensions
    for(i in names(r)){
        expect_true(all(c("_index", "_version") %in% names(r[[i]])))
        # select cols with "_source." in the name and remove the rest
        colnames <- names(r[[i]]) %>% 
        lapply(function(x){ if(startsWith(x, "_source.")) x else NULL }) %>% 
        lapply(function(x){ gsub("_source.", "", x) }) %>% 
        unlist(use.names = FALSE)
        expect_true(all(names(ds[[i]]) %in% colnames))
    }
})

test_that("kibior::pull, nominal, single index, columns NULL is complete", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    for(d in names(ds)){
        r <- kc$pull(d, columns = NULL)[[d]]
        expect_setequal(names(r), c("kid", names(ds[[d]])) )
    }
})

test_that("kibior::pull, nominal, multiple indices, columns NULL is complete", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # 
    r <- kc$pull(c("starwars", "storms"), columns = NULL)
    expect_length(r, 2)
    for(i in names(r)){
        expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])) )
    }
})

test_that("kibior::pull, nominal, indices via pattern, columns NULL is complete", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # 
    r <- kc$pull("s*", columns = NULL)
    expect_length(r, 2)
    for(i in names(r)){
        expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])) )
    }
})



test_that("kibior::pull, nominal, one index, select some columns, without metadata", {
    # push data
    remove_all_indices()
    selected_columns <- list()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
        # select two columns randomly
        selected_columns[[d]] <- names(ds[[d]]) %>% sample(2)
    }
    expect_setequal(kc$list(), names(ds))
    for(d in names(ds)){
        # ask for randomly selected columns
        r <- kc$pull(d, columns = selected_columns[[d]])[[d]]
        expect_setequal(names(r), selected_columns[[d]])
    }
})

test_that("kibior::pull, nominal, one index, select some columns, with metadata", {
    # push data
    remove_all_indices()
    selected_columns <- list()
    selected_columns_with_source <- list()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
        # select two columns randomly
        selected_columns[[d]] <- names(ds[[d]]) %>% sample(2) 
        # force the name to be "_source.<field>" to cmp more easily after
        selected_columns_with_source[[d]] <- selected_columns[[d]] %>% 
        lapply(function(x){ paste0("_source.", x) }) %>% 
        unlist(use.names = FALSE)
    }
    expect_setequal(kc$list(), names(ds))
    for(d in names(ds)){
        # select two mentionned columns
        r <- kc$pull(d, columns = selected_columns[[d]], keep_metadata = TRUE)[[d]]
        # compare with "_source.<field>"
        expect_true(all(selected_columns_with_source[[d]] %in% names(r)))
    }
})



test_that("kibior::pull, nominal, all indices, select one field only present in two datasets, without metadata", {
    # push data
    remove_all_indices()
    selected_columns <- list()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # we want to test specific names of columns
    # the field "name" is present in "starwars" and "storms" dataset only
    # should no get anything from "storms" dataset
    r <- kc$pull("*", columns = "name", keep_metadata = FALSE)
    expect_length(r, 2)
    expect_true("name" %in% names(r$starwars))
    expect_true(!("_index" %in% names(r$starwars)))
    expect_true("name" %in% names(r$storms))
    expect_true(!("_index" %in% names(r$storms)))

})

test_that("kibior::pull, nominal, all indices, select one field only present in two datasets, with metadata", {
    # push data
    remove_all_indices()
    selected_columns <- list()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # we want to test specific names of columns
    # the field "name" is present in "starwars" and "storms" dataset only
    # should no get anything from "storms" dataset
    r <- kc$pull("*", columns = "name", keep_metadata = TRUE)
    expect_length(r, 2)
    expect_true("_source.name" %in% names(r$starwars))
    expect_true("_index" %in% names(r$starwars))
    expect_true("_source.name" %in% names(r$storms))
    expect_true("_index" %in% names(r$storms))
})

# end pull



