library(testthat)
library(quicR)


# use_r("organize_tables")

for (i in c("test", "test_no_meta")) {
  file <- paste0("input_files/", i, ".xlsx")
  df_list <- organize_tables(file)

  test_that(
    "organize_tables returns list?",
    {
      expect_type(df_list, "list")
    }
  )

  test_that(
    "organize_tables returns list of tibbles?",
    {
      expect_true(
        class(df_list[[1]])[1] == "tbl_df" &
          class(df_list[[1]])[2] == "tbl" &
          class(df_list[[1]])[3] == "data.frame"
      )
    }
  )
}

test_that(
  "organize_tables accepts 384 as plate arg?",
  {
    expect_type(
      organize_tables("input_files/test384.xlsx", plate = 384), "list"
    )
  }
)

