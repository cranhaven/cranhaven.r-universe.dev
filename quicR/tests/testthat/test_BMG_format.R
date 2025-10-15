library(testthat)
library(quicR)


# use_r("BMG_format")

test_file <- "BMG_formatting/plate_layout.csv"
ref_file <- "BMG_formatting/formatted.txt"

test_that(
  "BMG_format returns character string.",
  {
    expect_type(BMG_format(test_file), "character")
  }
)

test_that(
  "BMG_format output matches formatted file.",
  {
    expect_equal(
      data.frame(V1 = BMG_format(test_file)),
      read.delim(ref_file, header = FALSE)
    )
  }
)
