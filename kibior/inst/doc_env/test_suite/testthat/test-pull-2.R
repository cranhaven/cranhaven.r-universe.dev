

context("Move data - pull - p2")


testthat::setup({
    # remove indices if they exist
    remove_all_indices()
})

testthat::teardown({
    # remove indices if they exist
    remove_all_indices()
})







# start pull ----


test_that("kibior::pull, more than 10k results asked per bulk", {
    # push data
    remove_all_indices()
    sw <- "starwars"
    ds[[sw]] %>% kc$push(sw)
    #
    res <- kc$pull(sw, bulk_size = 10001L)[[sw]]
    res2 <- kc$pull(sw)[[sw]]
    #
    expect_setequal(names(res), names(res2))
    expect_equal(nrow(res), nrow(res2))
})



test_that("kibior::pull, nominal simple case, single index, no impact regarding bulk_size", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # 
    for(b in c(10, 5000, 10000000)){
        for(d in names(ds)){
            r <- kc$pull(d, bulk_size = b)[[d]]
            expect_setequal(names(r), kc$columns(d)[[d]])
            expect_equal(nrow(r), kc$count(d)[[d]])
        }
    }
})

test_that("kibior::pull, nominal simple case, multiple indices, no impact regarding bulk_size", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # 
    for(b in c(10, 5000, 10000000)){
        r <- kc$pull(c("starwars", "storms"), bulk_size = b)
        expect_length(r, 2)
        # dimension
        expect_setequal(names(r$starwars), c("kid", names(ds[["starwars"]])))
        expect_setequal(names(r$storms), c("kid", names(dplyr::storms)))
        expect_equal(nrow(r$starwars), nrow(ds[["starwars"]]))
        expect_equal(nrow(r$storms), nrow(dplyr::storms))
    }
})

test_that("kibior::pull, nominal simple case, get via pattern, no impact regarding bulk_size", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # 
    for(b in c(10, 5000, 10000000)){
        # get starwars and storms datasets
        r <- kc$pull("s*", bulk_size = b)
        expect_length(r, 2)
        # dimension
        expect_setequal(names(r$starwars), c("kid", names(ds[["starwars"]])))
        expect_setequal(names(r$storms), c("kid", names(dplyr::storms)))
        expect_equal(nrow(r$starwars), nrow(ds[["starwars"]]))
        expect_equal(nrow(r$storms), nrow(dplyr::storms))
    }
})

test_that("kibior::pull, single index, nominal expected max_size asked", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # arbitrary sizes
    for(s in c(10, 5000, 10000000)){
        for(d in names(ds)){
        r <- kc$pull(d, max_size = s)[[d]]
        expect_setequal(names(r), c("kid", kc$columns(d)[[d]]))
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

test_that("kibior::pull, multiple indices, nominal expected max_size asked", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # arbitrary sizes
    for(s in c(10, 5000, 10000000)){
        r <- kc$pull(c("starwars", "storms"), max_size = s)
        expect_length(r, 2)
        # dimensions
        expect_setequal(names(r$starwars), c("kid", names(ds[["starwars"]])))
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

test_that("kibior::pull, indices via pattern, nominal expected max_size asked", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # arbitrary sizes
    for(s in c(10, 5000, 10000000)){
        r <- kc$pull("s*", max_size = s)
        expect_length(r, 2)
        # dimensions
        expect_setequal(names(r$starwars), c("kid", names(ds[["starwars"]])))
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

test_that("kibior::pull, nominal too short scroll timer", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # really short timer 1 nanosecond so the scroll connection expires
    # and cannot retrieve the data
    for(d in names(ds)){
        expect_error(kc$pull(d, scroll_timer = "1ns"))
    }
})

test_that("kibior::pull, wrong scroll timer", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    for(d in names(ds)){
        expect_error(kc$pull(d, scroll_timer = "NOOOOOPE"))
    }
})



# end pull



