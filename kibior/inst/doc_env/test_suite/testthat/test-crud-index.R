

context("CRUD index")



testthat::setup({
    # remove indices if they exist
    remove_all_indices()
})

testthat::teardown({
    # remove indices if they exist
    remove_all_indices()
})




# start has ----

test_that("kibior::has, nominal case", {
    res <- kc$create(single_index_name, force = TRUE)
    expect_true(res[[single_index_name]])
    expect_true(kc$has(single_index_name)[[single_index_name]])
    res <- kc$delete(single_index_name)
    expect_false(kc$has(single_index_name)[[single_index_name]])
})

test_that("kibior::has, null arg", {
    expect_error(kc$has(NULL), "`index_name` must be character.", fixed=TRUE)
})

test_that("kibior::has, wrong types args", {
    expect_error(kc$has(list()))
    expect_error(kc$has(c())) # == NULL
    expect_error(kc$has(NA))
    expect_error(kc$has(list("aaa", "bbb")))
    expect_error(kc$has(list(aa="aaa", bb="bbb")))
})

test_that("kibior::has, multiple test vector", {
    res <- kc$create(multiple_indice_names, force = TRUE)
    res %>% unlist(use.names = FALSE) %>% all() %>% expect_true()
    #
    res <- kc$has(multiple_indice_names)
    res %>% unlist(use.names = FALSE) %>% all() %>% expect_true()
    #
    res <- kc$delete(multiple_indice_names)
    res %>% unlist(use.names = FALSE) %>% all() %>% expect_true()
    #
    res <- kc$has(multiple_indice_names)
    res %>% unlist(use.names = FALSE) %>% all() %>% expect_false()
})

# end has






# start create ----

test_that("kibior::create, nominal case single creation", {
    remove_all_indices()
    res <- kc$create(single_index_name)[[single_index_name]]
    expect_true(res)
})

test_that("kibior::create, nominal case multiple creations", {
    res <- kc$create(multiple_indice_names)
    expect_setequal(names(res), multiple_indice_names)
    expect_equal(length(res), length(multiple_indice_names))
    res %>% unlist(use.names = FALSE) %>% all() %>% expect_true()
})

test_that("kibior::create, multiple creations with same name sequentially", {
    remove_all_indices()
    # first create ok
    res <- kc$create(single_index_name)
    expect_equal(names(res), single_index_name)
    res %>% unlist(use.names = FALSE) %>% expect_true()
    # second false
    res <- kc$create(single_index_name)
    expect_equal(names(res), single_index_name)
    res %>% unlist(use.names = FALSE) %>% expect_false()
})

test_that("kibior::create, multiple creations with same name same time", {
    remove_all_indices()
    #
    m <- c(single_index_name, single_index_name)
    res <- kc$create(m)
    expect_equal(names(res), single_index_name)
    res %>% unlist(use.names = FALSE) %>% expect_false()
})

test_that("kibior::create, pattern fail", {
    remove_all_indices()
    expect_error(kc$create(index_name = "asdf*"))
    expect_error(kc$create("asdf*"))
})

test_that("kibior::create, null arg", {
    remove_all_indices()
    expect_error(kc$create(index_name = NULL))
    expect_error(kc$create(NULL))
})

test_that("kibior::create, wrong types args", {
    remove_all_indices()
    expect_error(kc$create(123)) # no type coercion
    expect_error(kc$create(list()))
    expect_error(kc$create(c()))
    expect_error(kc$create(NA))
    expect_error(kc$create(list("aaa", "bbb")))
    expect_error(kc$create(list(aa="aaa", bb="bbb")))
})


# end create




# start recreate ----

test_that("kibior::recreate, no index yet", {
    remove_all_indices()
    # recreate with no index 
    res <- kc$create(single_index_name, force = TRUE)
    expect_true(res[[single_index_name]])
    expect_setequal(kc$list(), single_index_name)
})

test_that("kibior::recreate, nominal case", {
    remove_all_indices()
    # create
    res <- kc$create(single_index_name)
    expect_true(res[[single_index_name]])
    # recreate
    res <- kc$create(single_index_name, force = TRUE)
    expect_true(res[[single_index_name]])
    expect_setequal(kc$list(), single_index_name)
})

test_that("kibior::recreate, null args", {
    expect_error(kc$create(index_name = NULL, force = TRUE))
    expect_error(kc$create(NULL, force = TRUE))
})

# end recreate






# start delete ----

test_that("kibior::delete, null arg", {
    expect_error(kc$delete(), "argument \"index_name\" is missing, with no default")
    expect_error(kc$delete(NULL), "`index_name` must be character.")
})

test_that("kibior::delete, wrong types args", {
    expect_error(kc$delete(list()))
    expect_error(kc$delete(c())) # == NULL
    expect_error(kc$delete(NA))
    expect_error(kc$delete(list("aaa", "bbb")))
    expect_error(kc$delete(list(aa="aaa", bb="bbb")))
})

test_that("kibior::delete, nominal case single index", {
    remove_all_indices()
    # create
    res <- kc$create(single_index_name)
    expect_true(res[[single_index_name]])
    expect_true(kc$has(single_index_name)[[single_index_name]])
    # delete
    res2 <- kc$delete(single_index_name)
    expect_true(res2[[single_index_name]])
    expect_false(kc$has(single_index_name)[[single_index_name]])
})

test_that("kibior::delete, nominal case multiple indices", {
    remove_all_indices()
    res <- kc$create(multiple_indice_names, force = TRUE)
    kc$has(names(res)) %>% unlist(use.names = FALSE) %>% all() %>% expect_true()
    # del all indices
    res2 <- kc$delete(names(res))
    res2 %>% unlist(use.names = FALSE) %>% all() %>% expect_true()
    res2 %>% names() %>% kc$has() %>% unlist(use.names = FALSE) %>% all() %>% expect_false()
})

test_that("kibior::delete, try deleting something not there", {
    remove_all_indices()
    # del all indices
    expect_null(kc$delete(single_index_name))
})

# end delete




# start list ----

test_that("kibior::list, null arg, no index", {
    remove_all_indices()
    expect_null(kc$list())
})

test_that("kibior::list, nominal case, one index", {
    remove_all_indices()
    # add one
    res <- kc$create(single_index_name)
    expect_true(kc$has(names(res))[[single_index_name]])
    expect_equal(names(res), single_index_name)
    # list
    res2 <- kc$list()
    expect_equal(res2, names(res))
})

test_that("kibior::list, nominal case, some indices", {
    remove_all_indices()
    res <- kc$create(multiple_indice_names)
    kc$has(names(res)) %>% unlist(use.names = FALSE) %>% all() %>% expect_true()
    #
    expect_setequal(kc$list(), names(res))
})

# end list

