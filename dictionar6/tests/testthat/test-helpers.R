test_that("assert_named_list", {
    expect_error(assert_named_list(1), "must be a list")
    expect_error(assert_named_list(list(1)), "'x' must have unique")
    expect_error(assert_named_list(list(a = 1, a = 2)), "'x' must have unique")
})

test_that("where_env", {
    e1 <- list2env(list(a = 1))
    e2 <- list2env(list(b = 2), parent = e1)
    expect_equal(where_env("a", e2), e1)
})

test_that("update_env_value", {
    e1 <- list2env(list(a = 1))
    e2 <- list2env(list(b = 2), parent = e1)
    update_env_value("a", 3, e2)
    expect_equal(e1$a, 3)

    expect_error(update_env_value("e", 3, e2), "'e' does not exist")
})

test_that("rename_env_item", {
    e1 <- list2env(list(a = 1))
    e2 <- list2env(list(b = 2), parent = e1)

    expect_error(rename_env_item("a", "b", e2), "already exists")
    expect_error(rename_env_item("e", "d", e2), "'e' does not exist")

    rename_env_item("a", "f", e2)
    expect_equal(e1$a, NULL)
    expect_equal(e1$f, 1)
})
