test_that("if no packages, shows nothing", {
  expect_null(cat(redistverse_attach_message(character())))
})

test_that("message lists all core tidyverse packages", {
  expect_true(all(
    sapply(core_pkgs, grepl, redistverse_attach_message(core_pkgs))
  ))
})

test_that("logo prints", {
  expect_snapshot(redistverse_logo())
})
