
test_that("Utilities works fine", {
  capture.output({
  testthat::expect_error(
    stopf("Error: %s", "problem"),
    regexp = "Error: problem",
    fixed = TRUE
  )
  testthat::expect_error(
    stopaste("Error:", "problem", sep = " "),
    regexp = "Error: problem",
    fixed = TRUE
  )
  testthat::expect_error(
    stopaste0("Error: ", "problem"),
    regexp = "Error: problem",
    fixed = TRUE
  )
  testthat::expect_output(
    printf("Info: %s", "message"),
    regexp = "Info: message",
    fixed = TRUE
  )
  testthat::expect_output(
    printfp(list(
      "Info: %s",
      "Sur: %s",
      "Plusieurs: %s",
      "Lignes: %s"
    ),
      "1",
      "2",
      "3",
      "4"
    ),
    regexp = "Info: 1 Sur: 2 Plusieurs: 3 Lignes: 4",
    fixed = TRUE
  )
  testthat::expect_output(
    printp(
      "Info", "sur", "plusieurs", "lignes",
      sep = " "
    ),
    regexp = "Info sur plusieurs lignes",
    fixed = TRUE
  )
  })
})

testthat::test_that("Parameter checkers works fine", {
  uwu <- function(x) {
    check_parameter_type(x, "character")
  }
  `owo` <- function(x) { #nolint
    check_parameter_length(x, 1)
  }
  testthat::expect_null(uwu(""))
  testthat::expect_null(owo(""))
  testthat::expect_error(
    uwu(42),
    regexp = "The 'x' parameter for uwu must be a character, not a numeric.",
    fixed = TRUE
  )
  testthat::expect_error(
    owo(c("", "")),
    regexp = "The 'x' parameter for owo must be 1 element long, not 2.",
    fixed = TRUE
  )

  x <- function(text, logical, number) {
    check_one_character(text)
    check_one_logical(logical)
    check_one_numeric(number)
  }
  testthat::expect_error(
    x(1, TRUE, 12),
    regexp = "The 'text' parameter for x must be a character, not a numeric.",
    fixed = TRUE
  )
  testthat::expect_error(
    x("1", "TRUE", 12L),
    regexp = "The 'logical' parameter for x must be a logical, not a character",
    fixed = TRUE
  )
  testthat::expect_error(
    x("1", TRUE, "12L"),
    regexp = "The 'number' parameter for x must be a numeric, not a character.",
    fixed = TRUE
  )

  ## checking each function generated programmatically that does
  ## parameters checking
  check_one_list_of_type <- list(
    complex = complex(imaginary = 42),
    character = "uwu",
    numeric = 1.5,
    integer = as.integer(1.5),
    logical = TRUE
  )
  for (type_name in names(check_one_list_of_type)) {
    checker <- get(sprintf("check_one_%s", type_name))
    value <- check_one_list_of_type[[type_name]]
    testthat::expect_no_error(
      (function(x) checker(x, or_null = TRUE))(NULL)
    )
    testthat::expect_no_error(
      (function(x) checker(x, or_null = TRUE))(value)
    )
    testthat::expect_no_error(
      (function(x) checker(x, or_more = TRUE))(c(value, value))
    )
    testthat::expect_error(
      (function(x) checker(x))(c(value, value)),
      regexp = paste0(
        "The 'x' parameter for (function(x) checker(x)) ",
        "must be 1 element long, not 2."
      ),
      fixed = TRUE
    )
  }
})
