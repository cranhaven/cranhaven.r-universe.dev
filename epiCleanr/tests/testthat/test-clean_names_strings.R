# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

# Test for basic transformations and styles
testthat::test_that("Basic transformations and styles work correctly", {
  testthat::expect_equal(clean_names_strings("Hello World"),
                         "hello_world")
  testthat::expect_equal(clean_names_strings("Hello World",
                                             style = "camel_case"),
                         "helloWorld")
  testthat::expect_equal(clean_names_strings("Hello World!",
                                             style = "simple_clean"),
                         "helloworld")
})

# Test for unsupported style
testthat::test_that("Unsupported style raises error", {
  testthat::expect_error(clean_names_strings("Hello World",
                                             style = "unsupported_style"))
})

# Test for character vectors
testthat::test_that("Works on character vectors", {
  testthat::expect_equal(clean_names_strings(c("Hello World", "Another Name")),
                         c("hello_world", "another_name"))
})

# Test for data frames
testthat::test_that("Works on data frames", {
  df <- data.frame("Hello World" = 1, "Another Name" = 2)
  cleaned_df <- clean_names_strings(df)
  testthat::expect_equal(names(cleaned_df), c("hello_world", "another_name"))
})

# Test for tibbles
testthat::test_that("Works on tibbles", {
  tb <- tibble::tibble("Hello World" = 1, "Another Name" = 2)
  cleaned_tb <- clean_names_strings(tb)
  testthat::expect_equal(names(cleaned_tb), c("hello_world", "another_name"))
})

# Test for matrices
testthat::test_that("Works on matrices", {
  mat <- matrix(1:4, ncol = 2)
  colnames(mat) <- c("Hello World", "Another Name")
  cleaned_mat <- clean_names_strings(mat)
  testthat::expect_equal(colnames(cleaned_mat),
                         c("hello_world", "another_name"))
})

# Test for lists
testthat::test_that("Works on lists", {
  lst <- list("Hello World" = 1, "Another Name" = 2)
  cleaned_lst <- clean_names_strings(lst)
  testthat::expect_equal(names(cleaned_lst), c("hello_world", "another_name"))
})

# Test for unsupported data types
testthat::test_that("Unsupported data types raise error", {
  testthat::expect_error(clean_names_strings(TRUE), "Unsupported input type.")
})
