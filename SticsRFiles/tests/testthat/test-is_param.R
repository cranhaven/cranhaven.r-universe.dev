context("No param")
test_that("error", {
  expect_error(is_stics_param())
})

context("One one more existing parameters ")
test_that("exist parameter(s)", {
  expect_true(is_stics_param("cfes"))
  expect_vector(is_stics_param( param = c("cfes", "mulchbat")),
                ptype = NULL, size = 2)
  expect_true(all(is_stics_param( param = c("cfes", "mulchbat"))))

})

context("Exist param ")
test_that("unknown parameter", {
  expect_false(is_stics_param("unknown_parameter"))
  expect_vector(is_stics_param( param = c("cfes", "unknown_parameter")),
                ptype = NULL, size = 2)
  expect_false(all(is_stics_param( param = c("cfes", "unknown_parameter"))))
})

context("Exist param for 2 versions")
test_that("existing parameter", {
  expect_true(is_stics_param("cfes", stics_version = "V9.2"))
  expect_true(is_stics_param("cfes", stics_version = "V8.5"))
})

