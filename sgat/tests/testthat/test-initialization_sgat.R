skip_on_cran()
skip_if_offline(host = "r-project.org")

try(initialization_sgat(), silent = TRUE)

test_that("Server initializes", {
  expect_match(class(remDr), "remoteDriver")
})

test_that("canShowErrorClass", {
  status <- remDr$showErrorClass()$status
  expect_equal(status, 0L)
})

test_that("canShowRemoteDriver", {
  expect_equal(remDr$show()$browserName, "firefox")
})

test_that("canGetStatus", {
  status <- remDr$getStatus()
  expect_identical(names(status), c("ready", "message", "build", "os", "java"))
})
