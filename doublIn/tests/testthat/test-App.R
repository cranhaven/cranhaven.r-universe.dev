# This file is for testing the applications in the apps/ directory.
# Following the explanation on https://rstudio.github.io/shinytest/articles/package.html#:~:text=You%20will%20need%20to%20add,section%20in%20your%20DESCRIPTION%20file.&text=When%20all%20of%20these%20items,or%20Build%20%2D%3E%20Check%20Package.

library(shinytest)

test_that("Visualize_contact_tracing_data() works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

 # expect_pass(
    testApp(test_path("app/"),
                      compareImages = FALSE)
    #)
})
