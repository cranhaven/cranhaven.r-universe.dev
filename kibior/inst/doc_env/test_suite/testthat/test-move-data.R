

context("Move data")


testthat::setup({
    # remove indices if they exist
    remove_all_indices()
})

testthat::teardown({
    # remove indices if they exist
    remove_all_indices()
})


# start export ----

test_that("kibior::export, wrong args", {
    expect_error(kc$export())
    expect_error(kc$export(data = NULL))
    expect_error(kc$export(data = ds[["starwars"]], filepath = NULL))
    expect_error(kc$export(ds[["starwars"]], NULL))
    expect_error(kc$export("sw*", filepath = temp_filepath)) # pattern search
})

test_that("kibior::export, force arg wrong arg", {
    expect_error(kc$export(ds[["starwars"]], temp_filepath, force = NULL))
    expect_error(kc$export(ds[["starwars"]], temp_filepath, force = c()))
    expect_error(kc$export(ds[["starwars"]], temp_filepath, force = list()))
    expect_error(kc$export(ds[["starwars"]], temp_filepath, force = 3))
    expect_error(kc$export(ds[["starwars"]], temp_filepath, force = 3.56))
    expect_error(kc$export(ds[["starwars"]], temp_filepath, force = "wesh"))
})

test_that("kibior::export, wrong elastic index name", {
    remove_all_indices()
    expect_error(kc$export("unknown-index", temp_filepath))
})

test_that("kibior::export, file already exists", {
    unlink(temp_filepath)
    res <- kc$export(ds[["starwars"]], temp_filepath)
    expect_equal(res, temp_filepath)
    expect_true(file.exists(temp_filepath))
    expect_equal(count_nb_lines(temp_filepath), nrow(ds[["starwars"]]) + 1)
    expect_error(kc$export(ds[["starwars"]], temp_filepath))
})

test_that("kibior::export, nominal case, export from memory", {
    unlink(temp_filepath)
    res <- kc$export(ds[["starwars"]], temp_filepath)
    expect_equal(res, temp_filepath)
    expect_true(file.exists(temp_filepath))
    expect_equal(count_nb_lines(temp_filepath), nrow(ds[["starwars"]]) + 1)
    expect_equal(kc$export(ds[["starwars"]], temp_filepath, force = TRUE), temp_filepath)
})

test_that("kibior::export, nominal case, export from elasticsearch", {
    unlink(temp_filepath)
    remove_all_indices()
    iname <- "sw"
    res <- kc$push(ds[["starwars"]], iname)
    expect_equal(res, iname)
    # 
    res <- kc$export(iname, temp_filepath)
    expect_equal(res, temp_filepath)
    expect_true(file.exists(temp_filepath))
    expect_equal(count_nb_lines(temp_filepath), nrow(ds[["starwars"]]) + 1)
})

# end export




# start import ----

test_that("kibior::import, wrong args", {
    remove_all_indices()
    expect_error(kc$export())
    expect_error(kc$export(c(), "ok"))
    expect_error(kc$export(list(), "ok"))
    expect_error(kc$export(list(123, "asd"), "ok"))
    expect_error(kc$export(123, "ok"))
    expect_error(kc$export(123.234, "ok"))
    expect_error(kc$export("filenotfound", "ok"))
    expect_error(kc$export(c("sadf", "sdf"), "ok"))
    # duplicate
    expect_error(kc$export(temp_filepath, push_index = NULL))
    expect_error(kc$export(temp_filepath))
    expect_error(kc$export(temp_filepath, push_index = 123))
    expect_error(kc$export(temp_filepath, push_index = 123.234))
    expect_error(kc$export(temp_filepath, push_index = c()))
    expect_error(kc$export(temp_filepath, push_index = c("new1", "new2")))
    expect_error(kc$export(temp_filepath, push_index = list(123, "sdf")))
})

test_that("kibior::import, nominal case", {
    remove_all_indices()
    unlink(temp_filepath)
    kc$export(ds[["starwars"]], temp_filepath)
    # no push
    res <- kc$import(filepath = temp_filepath)
    expect_equal(nrow(res), nrow(ds[["starwars"]]))
    expect_equal(ncol(res), ncol(ds[["starwars"]]))
    # push 
    res <- kc$import(filepath = temp_filepath, push_index = "starwars")
    expect_equal(nrow(res), nrow(ds[["starwars"]]))
    expect_equal(ncol(res), ncol(ds[["starwars"]]))
    expect_equal(kc$list(), "starwars")
    res <- kc$pull("starwars")[["starwars"]]
    expect_equal(nrow(res), nrow(ds[["starwars"]]))
    expect_equal(ncol(res), ncol(ds[["starwars"]]) + 1)
})

# end import






# start move ----


test_that("kibior::move, nominal case", {
    remove_all_indices()
    res <- ds[["starwars"]] %>% kc$push("sw")
    expect_equal(res, "sw")
    # 
    res <- kc$move("sw", "sw_moved")
    expect_equal(res, "sw_moved")
})

# end move





# start copy ----

test_that("kibior::move, nominal case", {
    remove_all_indices()
    res <- ds[["starwars"]] %>% kc$push("sw")
    expect_equal(res, "sw")
    # 
    res <- kc$copy("sw", "sw_moved")
    expect_equal(res, "sw_moved")
})

# end copy
