context("test_create_wkbk")
library(stringi)
make_df_list <- function(size) {
  df_list <- list(size)
  if (size <= 0L) {
    return(df_list)
  }
  for (i in seq_len(size)) {
    n <- sample(2L:10L, 2L, replace = TRUE)
    df <- data.frame(matrix(data = rnorm(n[1L] * n[2L]), ncol = n[1L]))
    df_list[[i]] <- df
  }
  names(df_list) <- paste0("A", seq_len(size))
  df_list
}
df_list <- make_df_list(3L)
file <- filePath <- file.path(tempdir(), "testFile.xlsx")
if (file.exists(file)) {
  file.remove(file)
}

test_that("create_wkbk recognizes wrong number of dataframes", {
  sheetnames <- names(df_list)[1L:2L]
  expect_error(
    create_wkbk(
      file = file, df_list = df_list,
      sheetnames = sheetnames, replace = TRUE
    ),
    stri_c(
      "Number of 'sheetnames' specified does not ",
      "equal the number of data frames in 'df_list'."
    )
  )
})
test_that("create_wkbk writes file", {
  sheetnames <- names(df_list)
  expect_false(file.exists(file))
  create_wkbk(
    file = file, df_list = df_list, sheetnames = sheetnames,
    replace = FALSE
  )
  expect_true(file.exists(file))
})
test_that("create_wkbk recognizes preexisting file", {
  sheetnames <- names(df_list)
  expect_warning(
    create_wkbk(
      file = file, df_list = df_list,
      sheetnames = sheetnames, replace = FALSE
    ),
    stri_c("testFile.xlsx exists and was not overwritten")
  )
})
test_that("create_wkbk recognizes wrong number of dataframes", {
  sheetnames <- names(df_list)
  create_wkbk(
    file = file, df_list = df_list,
    sheetnames = sheetnames, replace = TRUE
  )
  expect_true(file.exists(file))
  if (file.exists(file)) {
    file.remove(file)
  }
})
