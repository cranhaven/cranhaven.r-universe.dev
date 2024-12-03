

test_that("optparse everything", {

  args <- optparse_parameters(
    a_integer = optparse_integer(),
    a_float = optparse_numeric(),
    a_boolean = optparse_flag(),
    a_character = optparse_character(),
    a_list = optparse_list(of = "numeric"),
    a_char_list = optparse_list(of = "character"),
    a_int_list = optparse_list(of = "integer"),
    a_bool_list = optparse_list(of = "logical"),
    args = list(
      "--a-integer",
      "42",
      "--a-float",
      "3.14",
      "--a-boolean",
      "FALSE",
      "--a-character",
      "FALSE",
      "--a-list",
      "1.5,2,3",
      "--a-char-list",
      "1.5,2,3",
      "--a-int-list",
      "1.5,2,3",
      "--a-bool-list",
      "1,T,F,,0"
    )
  )

  testthat::expect_equal(args$a_integer, 42)
  testthat::expect_type(args$a_integer, "integer")
  testthat::expect_equal(args$a_float, 3.14)
  testthat::expect_type(args$a_float, "double")
  testthat::expect_equal(args$a_boolean, FALSE)
  testthat::expect_type(args$a_boolean, "logical")
  testthat::expect_equal(args$a_character, "FALSE")
  testthat::expect_type(args$a_character, "character")
  testthat::expect_equal(args$a_list, list(1.5, 2, 3))
  testthat::expect_type(args$a_list, "list")
  testthat::expect_equal(args$a_char_list, list("1.5", "2", "3"))
  testthat::expect_type(args$a_char_list, "list")
  testthat::expect_equal(
    args$a_bool_list,
    list(TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  testthat::expect_type(args$a_bool_list, "list")
  testthat::expect_equal(
    args$a_int_list,
    list(as.integer(1), as.integer(2), as.integer(3))
  )
  testthat::expect_type(args$a_int_list, "list")

  testthat::expect_error(
    optparse_parameters(a = optparse_flag(), args = list()),
    regexp = NA
  )
  testthat::expect_error(
    optparse_parameters(a = optparse_flag(), args = list("--args")),
    regexp = paste0(
      "Error in getopt(spec = spec, opt = args) : ",
      "long flag \"args\" is invalid"
    ),
    fixed = TRUE
  )

  testthat::expect_error(
    optparse_parameters(
      a_strange_list = optparse_list(of = "list"),
      args = list("--a-strange-list", "list(1, 2, 3),list(1, 3, 4)")
    ),
    regexp = "Unknown type: list. Cannot transform without a convertor",
    fixed = TRUE
  )

  testthat::expect_error(
    optparse_parameters(no_optparse = TRUE),
    regexp = paste(
      "To uses `optparse_parameters`, you need to install the",
      "\"optparse\" package or to add it to your tool's dependencies"
    ), fixed = TRUE
  )
})