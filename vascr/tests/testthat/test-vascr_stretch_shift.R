# # future::plan("multisession")
# library("progressr")
# library(tidyr)
# library(furrr)

test_that("resample stretching works", {
  
  data.df = growth.df %>% vascr_subset(unit = "R", frequency = "4000", sample =c(1,3,8), time = c(0,50))
  
  t1 = growth.df %>% vascr_subset(unit = "R", frequency = 4000, experiment = 1, sample = "10,000_cells + HCMEC D3_line") %>% vascr_summarise(level = "experiments")
  t2 = growth.df %>% vascr_subset(unit = "R", frequency = 4000, experiment = 1, sample = "30,000_cells + HCMEC D3_line") %>% vascr_summarise(level = "experiments")
  
  # expect_snapshot({stretch_cc(t1, t2)})
  
  # expect_snapshot(vascr_summarise_cc_stretch_shift(data.df, 8))
  
   expect_snapshot(vascr_summarise_cc_stretch_shift_stats(data.df, reference = 3))
   
   testthat::skip_on_ci()
  
  vdiffr::expect_doppelganger("stretch shift stats", vascr_plot_cc_stretch_shift_stats(data.df, reference = 8))
  vdiffr::expect_doppelganger("stretch shift stats all comps", vascr_plot_cc_stretch_shift_stats(data.df))
  
})