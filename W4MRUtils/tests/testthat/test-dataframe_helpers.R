

write_read_df <- function(
  ...,
  write_args = list(),
  read_args = list()
) {
  df <- do.call(data.frame, list(...))
  path <- tempfile()
  file.create(path)
  write_args$x <- df
  write_args$file <- path
  do.call(write.table, write_args)
  read_args$file <- path
  return(suppressWarnings(
    list(
      original = df,
      actual = do.call(df_read_table, read_args)
    )
  ))
}

test_that("Testing df_is", {
  testthat::expect_true(df_is(data.frame(a = c(1)), "numeric"))
  testthat::expect_false(df_is(data.frame(a = c(1)), "character"))
  testthat::expect_false(df_is(
    data.frame(a = c(1), b = c("test")),
    "numeric"
  ))
  testthat::expect_false(df_is(
    data.frame(a = c(2, 1), b = c(TRUE, FALSE)),
    "numeric"
  ))
})

test_that("Testing df_force_numeric", {
  some_integers <- as.integer(c(1, 2))
  testthat::expect_false(
    df_is(
      df_force_numeric(data.frame(a = c("1.2"))),
      "numeric"
    )
  )
  testthat::expect_false(
    df_is(
      df_force_numeric(data.frame(a = c("1", "2"))),
      "numeric"
    )
  )
  testthat::expect_false(
    df_is(
      df_force_numeric(
        data.frame(a = c("1", "2"), b = c("3", "4")),
        cols = c("a")
      ),
      "numeric"
    )
  )
  testthat::expect_true(
    df_is(
      df_force_numeric(
        data.frame(a = some_integers, b = c("3", "4")),
        cols = c("a")
      )[, "a"],
      "numeric"
    )
  )
  testthat::expect_true(
    df_is(
      df_force_numeric(
        data.frame(a = c(1, 2), b = c("3", "4")),
        cols = c("a")
      )[, "a"],
      "numeric"
    )
  )
  testthat::expect_null(df_force_numeric(NULL))
  testthat::expect_identical(df_force_numeric(data.frame()), data.frame())
})

test_that("Testing df_read_table", {
  result <- write_read_df(
    a = c(6, 7, 8), b = c(4, 5, 6),
    read_args = list(force_numeric = TRUE)
  )
  testthat::expect_identical(result$original, result$actual)

  result <- write_read_df(
    a = c(6, 7, 8), b = c(4, 5, 6),
    read_args = list(force_numeric = FALSE)
  )
  testthat::expect_false(identical(result$original, result$actual))

  result <- write_read_df(
    a = c("7", "8", "9"), b = c(4, 5, 6),
    read_args = list(
      colClasses = c(a = "character", b = "numeric"),
      header = TRUE
    ),
    write_args = list(row.names = FALSE)
  )
  testthat::expect_identical(result$original, result$actual)

  result <- write_read_df(
    a = c("7", "8", "9"), b = c(4, 5, 6),
    write_args = list(row.names = FALSE),
    read_args = list(
      colClasses = c(a = "character", b = "numeric"),
      header = TRUE,
      force_numeric = "a"
    )
  )
  testthat::expect_identical(
    data.frame(a = c(7, 8, 9), b = c(4, 5, 6)),
    result$actual
  )

  result <- write_read_df(
    a = c("7", "8", "9"), b = c("4", "5", "6"),
    write_args = list(row.names = FALSE),
    read_args = list(
      colClasses = c(a = "character", b = "character"),
      header = TRUE
    )
  )
  testthat::expect_identical(result$original, result$actual)
})
