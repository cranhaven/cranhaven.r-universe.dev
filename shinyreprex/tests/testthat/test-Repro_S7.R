test_that("Repro object can be initialised without error", {
  expect_s7_class(Repro(), Repro)
})

test_that("Code added to the Repro object are available in the script", {
  my_code <- list(
    my_reactive = str2lang("my_reactive <- shiny::reactive(iris)"),
    my_value = str2lang("my_value <- 4.5")
  )
  my_repro <- Repro()

  my_repro@code <- my_code
  expect_identical(my_repro@code, my_code)
  expect_identical(my_repro@calls, c("my_reactive <- shiny::reactive(iris)", "my_value <- 4.5"))
})

test_that("Pre-requesites added to the Repro object are available in the script", {
  my_code <- list(
    my_reactive = str2lang("my_reactive <- shiny::reactive(iris)"),
    my_value = str2lang("my_value <- 4.5")
  )
  my_repro <- Repro()

  my_repro@prerequisites <- my_code
  expect_identical(my_repro@prerequisites, my_code)
  expect_identical(my_repro@calls, c("my_reactive <- shiny::reactive(iris)", "", "my_value <- 4.5", ""))
})

test_that("Duplicate pre-requesites are not added to the Repro object", {
  my_code <- list(
    my_reactive = str2lang("my_reactive <- shiny::reactive(iris)"),
    my_value = str2lang("my_value <- 4.5")
  )
  my_repro <- Repro()

  my_repro@prerequisites <- my_code
  my_repro@prerequisites <- list(my_value = str2lang("my_value <- 2.1"))
  expect_identical(my_repro@prerequisites, my_code)
  expect_identical(my_repro@calls, c("my_reactive <- shiny::reactive(iris)", "", "my_value <- 4.5", ""))
})

test_that("Packages added to a Repro object are unique, but retain order", {
  my_repro <- Repro(packages = c("rlang", "testthat", "rlang"))
  expect_identical(my_repro@packages, c("rlang", "testthat"))
  expect_identical(my_repro@calls, c("library(rlang)", "library(testthat)", ""))

  my_repro@packages <- c("purrr", "rlang")
  expect_identical(my_repro@packages, c("rlang", "testthat", "purrr"))
})
