test_that("fortify functions", {
  df <- loewesadditivity::rh5_ama1ron2
  df$dose_A <- df$RH5
  df$dose_B <- df$AMA1RON2
  fortified_df <- fortify_gia_data(df)
  expect_equal(dim(fortified_df), c(72, 8))

  df <- loewesadditivity::cyrpa_ripr
  df$dose_A <- df$CyRPA
  df$dose_B <- df$RIPR
  fortified_df <- fortify_gia_data(df)
  expect_equal(dim(fortified_df), c(72, 8))
})
