library(testthat)
library(quicR)





for (file in c("input_files/test.xlsx", "input_files/test2.xlsx", "input_files/test3.xlsx")) {
  data <- readxl::read_xlsx(file, sheet = 2)

  test_that(
    "get_real accepts Excel file as input?",
    {
      expect_type(get_real(file), "list")
    }
  )

  test_that(
    "get_real accepts dataframe as input?",
    {
      expect_type(get_real(data), "list")
    }
  )

  test_that(
    "get_real returns list of dataframes?",
    {
      expect_true(is.data.frame(get_real(data)[[1]]))
    }
  )
}
