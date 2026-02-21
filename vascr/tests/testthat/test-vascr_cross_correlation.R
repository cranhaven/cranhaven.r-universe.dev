test_that("ccf functions work", {
  
  
  # Produce the CC plots to work with
  data.df = vascr::growth.df %>% vascr_subset(unit = "R", frequency = 4000, sampleid = c(1,4), experiment = c(1,2), time = c(1,10), well = c("A02", "A03", "D01", "D06"))
  expect_snapshot({vascr_cc(data.df)})
  
  data.df = vascr::growth.df %>% vascr_subset(unit = "R", frequency = 4000, sampleid = c(1,4,7), time = c(1,20))
  
  suppressMessages({cc_data = vascr_cc(data.df, reference = "35,000_cells + HCMEC D3_line")})
  expect_snapshot(cc_data)
  
  suppressMessages({cc_data = vascr_cc(data.df, c("35,000_cells + HCMEC D3_line", "5,000_cells + HCMEC D3_line"))})
  expect_snapshot(cc_data)

  suppressMessages({cc_data = vascr_cc(data.df)})
  expect_snapshot(cc_data)
  

  # Check summarisation
  expect_snapshot(vascr_summarise_cc(cc_data, "experiments"))
  expect_snapshot(vascr_summarise_cc(cc_data, "summary"))
  
  testthat::skip_on_ci()
  vdiffr::expect_doppelganger("Wells cc plot", cc_data %>% vascr_plot_cc())
  vdiffr::expect_doppelganger("Experiment cc plot", cc_data %>% vascr_summarise_cc("experiments") %>% vascr_plot_cc())
  vdiffr::expect_doppelganger("Summary cc plot", cc_data %>% vascr_summarise_cc("summary") %>% vascr_plot_cc())
  
  
  
  vdiffr::expect_doppelganger("Plot CC 1", vascr_plot_cc_stats(growth.df))
  vdiffr::expect_doppelganger("Plot CC 2", vascr_plot_cc_stats(growth.df, reference = "0_cells + HCMEC D3_line"))
  
  vdiffr::expect_doppelganger("Plot CC 3", vascr_plot_cc_stats(growth.df, pval = TRUE))
  vdiffr::expect_doppelganger("Plot CC 4", vascr_plot_cc_stats(growth.df, reference = "0_cells + HCMEC D3_line", pval = TRUE))
  
  vdiffr::expect_doppelganger("Plot CC 5", vascr_plot_cc_stats(growth.df, points = TRUE))
  vdiffr::expect_doppelganger("Plot CC 6", vascr_plot_cc_stats(data.df = growth.df, reference = "0_cells + HCMEC D3_line", points = TRUE))
  
  
})
