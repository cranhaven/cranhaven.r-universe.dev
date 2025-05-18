
stics_version <- get_stics_versions_compat()$latest_version

context("Exist param ")

test_that("exist parameter", {
  expect_true(exist_param_xml("codetemp", stics_version = stics_version))
  expect_true(exist_param_xml("codegdh", stics_version = stics_version))
  expect_true(exist_param_xml("codephot", stics_version = stics_version))
  expect_warning(exist_param_xml("codexxx", stics_version = stics_version))
  expect_true(exist_param_xml("code_acti_reserve",
                              stics_version = stics_version))
})
