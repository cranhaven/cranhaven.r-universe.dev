context("No var")
test_that("error", {
  expect_error(is_stics_var())
})

context("One one more existing variables ")
test_that("exist variable(s)", {
  expect_true(is_stics_var("albedolai"))
  expect_vector(is_stics_var( var = c("albedolai", "exolai")),
                ptype = NULL, size = 2)
  expect_true(all(is_stics_var( var = c("albedolai", "exolai"))))

})

context("Exist var ")
test_that("unknown variable", {
  expect_false(is_stics_var("unknown_variable"))
  expect_vector(is_stics_var( var = c("albedolai", "unknown_variable")),
                ptype = NULL, size = 2)
  expect_false(all(is_stics_var( var = c("albedolai", "unknown_variable"))))
})

context("Exist var for 2 versions")
test_that("existing variable", {
  expect_true(is_stics_var("albedolai", stics_version = "V9.2"))
  expect_true(is_stics_var("albedolai", stics_version = "V8.5"))
})

