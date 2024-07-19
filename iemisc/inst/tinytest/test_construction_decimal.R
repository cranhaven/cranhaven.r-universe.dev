test_construction_decimal <- function() {

ex_error <- character(0)

expect_equal(construction_decimal("2'-0\"", result = "traditional", output = "vector"), 2)
expect_equal(construction_decimal("1'-2 7/16\"", result = "librecad", output = "vector"), 14.4375, tolerance = 1e-06)
expect_equal(construction_decimal("0 6", result = "traditional", output = "vector"), 0.5)
expect_equal(construction_decimal("0 6", result = "librecad", output = "vector"), 6)
expect_error(construction_decimal(5, result = "traditional", output = "vector"))
expect_error(construction_decimal(ex_error, result = "traditional", output = "vector"))
expect_error(construction_decimal(NA, result = "traditional", output = "vector"))
expect_error(construction_decimal("feet", result = "traditional", output = "vector"))

  invisible(NULL)
}

test_construction_decimal()
