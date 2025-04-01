

context("Search")


# ---------------------------------------------------------------


testthat::setup({
    # remove indices if they exist
    remove_all_indices()
    # multiple (starwars, storms, storms) with their name
    selected_columns <- list()
    selected_columns_with_source <- list()
    for(d in names(ds)){
        # select two columns randomly
        selected_columns[[d]] <- ds[[d]] %>% 
            names() %>% 
            sample(2) 
        # force the name to be "_source.<field>" to cmp more easily after
        selected_columns_with_source[[d]] <- selected_columns[[d]] %>% 
            lapply(function(x){ 
                paste0("_source.", x) 
            }) %>% 
            unlist(use.names = FALSE)
    }
    # assign var to global env
    assign("selected_columns", selected_columns, envir = .GlobalEnv)
    assign("selected_columns_with_source", selected_columns_with_source, envir = .GlobalEnv)
})

testthat::teardown({
    remove_all_indices()
    # remove var from global env 
    rm(selected_columns, envir = .GlobalEnv)
    rm(selected_columns_with_source, envir = .GlobalEnv)
})


# ---------------------------------------------------------------




# start search ----


test_that("kibior::search, wrong args", {
    single_index_name <- ds[[1]]
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
    # index name
    for(i in a){
        if(!is.null(i) && !is.character(i)){
            expect_error(kc$search(index_name = i, head = FALSE))
        }
    }
    # bulk 
    for(i in a){
        expect_error(kc$search(single_index_name, bulk_size = i, head = FALSE))
    }
    # max size
    for(i in a){
        if(!is.null(i)){ # if max_size is not null, else it returns everything
            expect_error(kc$search(single_index_name, max_size = i, head = FALSE))
        }
    }
    # scroll timer
    for(i in a){
        expect_error(kc$search(single_index_name, scroll_timer = i, head = FALSE))
    }
    # keep metadata 
    for(i in a){
        if(!is.logical(i)){
            expect_error(kc$search(single_index_name, keep_metadata = i, head = FALSE))
        }
    }
    expect_error(kc$search(single_index_name, keep_metadata = NA, head = FALSE))
    # columns filters 
    for(i in a){
        # if columns is not null or string, else it returns everything
        if(!is.null(i) && !is.character(i)){ 
            expect_error(kc$search(single_index_name, columns = i, head = FALSE))
        }
    }
    # columns filters with metadata
    for(i in a){
        # if columns is not null or string, else it returns everything
        if(!is.null(i) && !is.character(i)){ 
        expect_error(kc$search(single_index_name, keep_metadata = TRUE, columns = i, head = FALSE))
        }
    }
    # query
    for(i in a){
        if(is.character(i) && i != "NOPE"){ # this works but returns no results
            expect_error(kc$search(single_index_name, query = i, head = FALSE))
        }
    }
    # head
    for(i in a){
        if(!is.logical(i)){ 
            expect_error(kc$search(single_index_name, head = i))
        }
    }
    expect_error(kc$search(single_index_name, head = NA))
})


test_that("kibior::search, query size error", {
    remove_all_indices()
    index <- names(ds)[[1]]
    ds[[1]] %>% kc$push(index)
    # produces string of length nb containing only "a"
    produce_str <- function(nb){
        replicate(nb, "a") %>% 
            unlist(use.names = FALSE) %>% 
            paste0(collapse = "")
    }
    # 
    for(i in c(10, 100, 1000, 3000)){
        p <- produce_str(i)
        # full query under 4096 bytes, so this return something (empty)
        res <- kc$search(index, query = p)[[index]]
        # expect_null(res)
        expect_equal(typeof(res), "list")
        expect_equal(class(res), "list")
        expect_length(res, 0)
    }
    # by default, ES limit is 4096 bytes
    expect_error(kc$search(index, query = produce_str(10000)))
})




test_that("kibior::search, nominal simple case, get one index", {
    remove_all_indices()
    purrr::imap(ds, kc$push)
    for(d in names(ds)){
        r <- kc$search(d, head = FALSE)[[d]]
        # columns
        expected_columns <- c("kid", names(ds[[d]]))
        expect_setequal(names(r), expected_columns)
        # dim
        expect_equal(nrow(r), nrow(ds[[d]]))
    }
})

test_that("kibior::search, nominal simple case, get two indices", {
    ds_names <- ds %>% 
        head(2) %>% 
        names()
    # get "starwars" and "storms" datasets
    r <- kc$search(ds_names, head = FALSE)
    expect_length(r, 2)
    expect_setequal(ds_names, names(r))
    # test names
    for(d in ds_names){
        expect_setequal(c("kid", names(ds[[d]])), names(r[[d]]))
    }
    # test dimensions
    for(d in ds_names){
        expect_equal(nrow(ds[[d]]), nrow(r[[d]]))
    }
})

test_that("kibior::search, nominal simple case, get indices via pattern", {
    # get "starwars" and "storms" datasets
    expected_ds <- c("starwars", "storms")
    r <- kc$search("s*", head = FALSE)
    expect_length(r, 2)
    expect_setequal(expected_ds, names(r))
    # test names
    for(d in expected_ds){
        expect_setequal(c("kid", names(ds[[d]])), names(r[[d]]))
    }
    # test dimensions
    for(d in expected_ds){
        expect_equal(nrow(ds[[d]]), nrow(r[[d]]))
    }
})

test_that("kibior::search, wrong index names", {
    #
    false_indices <- c("aaaa", "bbbb", "cccc")
    for(i in false_indices){
        expect_null(kc$search(i, head = TRUE))
        expect_null(kc$search(i, head = FALSE))
    }
})

test_that("kibior::search, nominal simple case, single index, no impact regarding bulk_size", {
    # 
    for(b in cpt_loop){
        for(d in names(ds)){
            r <- kc$search(d, bulk_size = b, head = FALSE)[[d]]
            #
            expect_setequal(names(r), c("kid", names(ds[[d]])))
            expect_equal(nrow(r), nrow(ds[[d]]))
        }
    }
})

test_that("kibior::search, nominal simple case, multiple indices, no impact regarding bulk_size", {
    # 
    for(b in cpt_loop){
        r <- kc$search(c("starwars", "storms"), bulk_size = b, head = FALSE)
        expect_length(r, 2)
        # dimension
        expect_setequal(names(r$starwars), c("kid", names(dplyr::starwars)))
        expect_setequal(names(r$storms), c("kid", names(dplyr::storms)))
        expect_equal(nrow(r$starwars), nrow(dplyr::starwars))
        expect_equal(nrow(r$storms), nrow(dplyr::storms))
    }
})

test_that("kibior::search, nominal simple case, get via pattern, no impact regarding bulk_size", {
    # 
    for(b in cpt_loop){
        # get starwars and storms datasets
        r <- kc$search("s*", bulk_size = b, head = FALSE)
        expect_length(r, 2)
        # dimension
        expect_setequal(names(r$starwars), c("kid", names(dplyr::starwars)))
        expect_setequal(names(r$storms), c("kid", names(dplyr::storms)))
        expect_equal(nrow(r$starwars), nrow(dplyr::starwars))
        expect_equal(nrow(r$storms), nrow(dplyr::storms))
    }
})

test_that("kibior::search, bulk_size vs. max_size", {
    # default value, bulk_size > max_size
    # bulk_size should be restricted to max_size
    r <- kc$search("s*", max_size = 10, head = FALSE)
    expect_setequal(names(r), names(ds))
    for(i in names(r)){
        expect_equal(nrow(r[[i]]), 10)
        expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])) )
    }
    # explicit: should behave the same
    r <- kc$search("s*", bulk_size = 5000, max_size = 10, head = FALSE)
    expect_setequal(names(r), c("starwars", "storms"))
    for(i in names(r)){
        expect_equal(nrow(r[[i]]), 10)
        expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])) )
    }
    # identical values bulk_Size == max_size
    index <- names(ds)[[1]]
    m <- 50
    r <- kc$search(index, bulk_size = m, max_size = m, head = FALSE)[[index]]
    expect_setequal(names(r), c("kid", names(ds[[1]])))
    s <- if(m > nrow(ds[[1]])) nrow(ds[[1]]) else m
    expect_equal(nrow(r), s)
})

test_that("kibior::search, single index, nominal expected max_size asked", {
    # arbitrary sizes
    for(s in cpt_loop){
        for(d in names(ds)){
            r <- kc$search(d, bulk_size = min(cpt_loop), max_size = s, head = FALSE)[[d]]
            expect_setequal(names(r), c("kid", names(ds[[d]])))
            # size
            co <- kc$count(d)[[d]]
            if(s > co){
                expect_equal(nrow(r), co)
            } else {
                expect_equal(nrow(r), s)
            }
        }
    }
})

test_that("kibior::search, multiple indices, nominal expected max_size asked", {
    # arbitrary sizes
    for(s in cpt_loop){
        r <- kc$search(c("starwars", "storms"), bulk_size = min(cpt_loop), max_size = s, head = FALSE)
        expect_length(r, 2)
        # dimensions
        expect_setequal(names(r$starwars), c("kid", names(dplyr::starwars)))
        expect_setequal(names(r$storms), c("kid", names(dplyr::storms)))
        swco <- kc$count("starwars")[["starwars"]]
        stco <- kc$count("storms")[["storms"]]
        if(s > swco){
            expect_equal(nrow(r$starwars), swco)
        } else {
            expect_equal(nrow(r$starwars), s)
        }
        if(s > stco){
            expect_equal(nrow(r$storms), stco)
        } else {
            expect_equal(nrow(r$storms), s)
        }
    }
})

test_that("kibior::search, indices via pattern, nominal expected max_size asked", {
    # arbitrary sizes
    for(s in cpt_loop){
        r <- kc$search("s*", bulk_size = min(cpt_loop), max_size = s, head = FALSE)
        expect_length(r, 2)
        # dimensions
        expect_setequal(names(r$starwars), c("kid", names(dplyr::starwars)))
        expect_setequal(names(r$storms), c("kid", names(dplyr::storms)))
        swco <- kc$count("starwars")[["starwars"]]
        stco <- kc$count("storms")[["storms"]]
        if(s > swco){
            expect_equal(nrow(r$starwars), swco)
        } else {
            expect_equal(nrow(r$starwars), s)
        }
        if(s > stco){
            expect_equal(nrow(r$storms), stco)
        } else {
            expect_equal(nrow(r$storms), s)
        }
    }
})

test_that("kibior::search, nominal too short scroll timer", {
    # really short timer 1 nanosecond so the scroll connection expires
    # and cannot retrieve the data
    for(d in names(ds)){
        expect_error(kc$search(d, scroll_timer = "1ns", head = FALSE))
    }
})

test_that("kibior::search, wrong scroll timer", {
    for(d in names(ds)){
        expect_error(kc$search(d, scroll_timer = "NOOOOOPE", head = FALSE))
    }
})

test_that("kibior::search, keep metadata, single index", {
    for(d in names(ds)){
        # ask meta
        r <- kc$search(d, keep_metadata = TRUE, head = FALSE)[[d]]
        expect_equal(r[["_index"]][[1]], d)
        expect_setequal(r[["_id"]], r[["_source.kid"]])
        # compare colnames with no metadata result
        rr <- kc$search(d, keep_metadata = FALSE, head = FALSE)[[d]]
        expect_equal(nrow(rr), nrow(r))
        colnames <- names(r) %>% 
            lapply(function(x){ if(startsWith(x, "_source.")) x else NULL }) %>% 
            lapply(function(x){ gsub("_source.", "", x) }) %>% 
            unlist(use.names = FALSE)
        expect_setequal(colnames, names(rr))
    }
})

test_that("kibior::search, keep metadata, multiple indices", {
    # ask meta
    r <- kc$search(c("starwars", "storms"), keep_metadata = TRUE, head = FALSE)
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

test_that("kibior::search, keep metadata, indices via pattern", {
    # ask meta
    r <- kc$search("s*", keep_metadata = TRUE, head = FALSE)
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

test_that("kibior::search, nominal, single index, columns NULL is complete", {
    for(d in names(ds)){
        r <- kc$search(d, columns = NULL, head = FALSE)[[d]]
        expect_setequal(names(r), c("kid", names(ds[[d]])) )
    }
})

test_that("kibior::search, nominal, multiple indices, columns NULL is complete", {
    # 
    r <- kc$search(c("starwars", "storms"), columns = NULL, head = FALSE)
    expect_length(r, 2)
    for(i in names(r)){
        expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])) )
    }
})

test_that("kibior::search, nominal, indices via pattern, columns NULL is complete", {
    # 
    r <- kc$search("s*", columns = NULL, head = FALSE)
    expect_length(r, 2)
    for(i in names(r)){
        expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])) )
    }
})

test_that("kibior::search, nominal, one index, select some columns, without metadata", {
    for(d in names(ds)){
        # ask for randomly selected columns
        r <- kc$search(d, columns = selected_columns[[d]], head = FALSE)[[d]]
        expect_setequal(names(r), selected_columns[[d]])
    }
})

test_that("kibior::search, nominal, one index, select some columns, with metadata", {
    for(d in names(ds)){
        # select two mentionned columns
        r <- kc$search(d, columns = selected_columns[[d]], keep_metadata = TRUE, head = FALSE)[[d]]
        # compare with "_source.<field>"
        expect_true(all(selected_columns_with_source[[d]] %in% names(r)))
    }
})

test_that("kibior::search, nominal, all indices, select one field only present in two datasets, without metadata", {
    # we want to test specific names of columns
    # the field "name" is present in "starwars" and "storms" dataset only
    # should no get anything from "storms" dataset
    r <- kc$search("*", columns = "name", keep_metadata = FALSE, head = FALSE)
    expect_length(r, length(names(ds)))

    expect_true("name" %in% names(r$starwars))
    expect_true(!("_index" %in% names(r$starwars)))

    expect_true("name" %in% names(r$storms))
    expect_true(!("_index" %in% names(r$storms)))

})

test_that("kibior::search, nominal, all indices, select one field only present in two datasets, with metadata", {
    # we want to test specific names of columns
    # the field "name" is present in "starwars" and "storms" dataset only
    # should no get anything from "storms" dataset
    r <- kc$search("*", columns = "name", keep_metadata = TRUE, head = FALSE)
    expect_length(r, length(names(ds)))

    expect_true("_source.name" %in% names(r$starwars))
    expect_true("_index" %in% names(r$starwars))

    expect_true("_source.name" %in% names(r$storms))
    expect_true("_index" %in% names(r$storms))
})


# HEAD

test_that("kibior::search, head search, one index", {
    # head on
    r <- kc$search("starwars", head = TRUE)
    expect_length(r, 1)
    expect_equal(nrow(r$starwars), kc$head_search_size)
    expect_setequal(names(r$starwars), c("kid", names(dplyr::starwars)))

    # head off
    r <- kc$search("starwars", head = FALSE)
    expect_length(r, 1)
    expect_equal(nrow(r$starwars), nrow(dplyr::starwars))
    expect_setequal(names(r$starwars), c("kid", names(dplyr::starwars)))
})

test_that("kibior::search, head search, all indices", {
    # head on
    r <- kc$search("*", head = TRUE)
    expect_length(r, length(ds))
    for(i in names(r)){
        expect_equal(nrow(r[[i]]), kc$head_search_size)
        expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])))
    }
    # head off
    r <- kc$search("*", head = FALSE)
    expect_length(r, length(ds))
    for(i in names(r)){
        expect_equal(nrow(r[[i]]), nrow(ds[[i]]))
        expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])))
    }
})

test_that("kibior::search, size to return", {
    # push a ds smaller than head size
    small <- ds[[1]] %>% head(kc$head_search_size - 2) # default is 5, -> 3
    small %>% kc$push("small")
    new_ds <- list("small" = small)
    # if head on, then 
    #     ds size > head size, ds are restricted to head size
    #     ds size < head size, ds are full
    r <- kc$search("*", head = TRUE)
    for(i in names(new_ds)){
        if(nrow(new_ds[[i]]) > kc$head_search_size){
            expect_equal(nrow(r[[i]]), kc$head_search_size)
        } else {
            expect_equal(nrow(r[[i]]), nrow(new_ds[[i]]))
        }
    }
    # if head off, and max size set them
    #     if max size >= ds size, then
    #         ds are full
    #     else
    #         ds are restricted to max size 
    for(m in c(2, 5, 100, 10500)){
        r <- kc$search("*", max_size = m, bulk_size = m, head = FALSE)
        if(nrow(new_ds[[i]]) > m){
            expect_equal(nrow(r[[i]]), m)
        } else {
            expect_equal(nrow(r[[i]]), nrow(new_ds[[i]]))
        }
    }

    # if head off, and no max_size then
    #     ds are full
    r <- kc$search("*", max_size = NULL, head = FALSE)
    for(i in names(new_ds)){
        expect_equal(nrow(r[[i]]), nrow(new_ds[[i]]))
    }
    # delete tmp
    kc$delete("small")
})

# end search
