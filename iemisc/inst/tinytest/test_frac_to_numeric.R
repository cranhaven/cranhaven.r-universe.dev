test_frac_to_numeric <- function() {

xx <- as.character(fractional::fractional(1:9 / 12))

xi <- fracture::fracture((50:65) / 12)

xyy <- fracture::fracture((1:11) / 12)

xft <- as.character(MASS::fractions((1:70) / 12))

pix <- "270/11"

expect_equal(frac_to_numeric(pix), 24.54545, tolerance = 1e-06)
expect_error(frac_to_numeric(xi))
expect_error(frac_to_numeric(xyy))
expect_error(frac_to_numeric(xft))
expect_error(frac_to_numeric(xx))

  invisible(NULL)
}

test_frac_to_numeric()
