context("Plot cofeatureR")

library("tibble")
library("dplyr")
in_df <- 
  tribble(
    ~feature, ~sampleID, ~type,          ~p_value, ~dir_flag,
    "RCOR1", "sampleA", "Deletion",      0.05,     1L,
    "NCOR1", "sampleC", "Deletion",      0.5,      2L,
    "LCOR",  "sampleB", "SNV",           0.25,     1L,
    "RCOR1", "sampleC", "Rearrangement", 0.01,     1L,
    "RCOR1", "sampleA", "SNV",           0.03,     2L,
    "RCOR1", "sampleC", "Rearrangement", 0.24,     2L,
    "RCOR1", "sampleC", "SNV",           0.89,     1L
  )

in_df <- mutate(in_df, p_value = -log10(p_value))
fill.colors <- c("Deletion" = "Blue", "Rearrangement" = "Green", "SNV" = "Red")

test_that("plot_cofeature_mat correctly adds tile borders", {
  border_cols <- c("black", "blue", "green")
  for (cur_border_col in border_cols) {
    cur_plot <- plot_cofeature_mat(in_df, tile.col = cur_border_col)
    expect_equal(
      cur_plot$layers[[1]]$aes_params$colour, cur_border_col,
      info = paste("Setting tile border color to", cur_border_col)
    )
  }
})
