# round_prob = function(test)
# {
#   test$Tukey.level = signif(test$Tukey.level,3)
#   test$p.adj = signif(test$p.adj,3)
#   return(test)
# }


test_that("Can make a significance table", {
  
  expect_snapshot(vascr_make_significance_table(growth.df, 50, "R", 4000, 0.95))
  expect_snapshot(vascr_make_significance_table(growth.df, 50, "R", 4000, 0.95, format = "Tukey_data"))
  
})


test_that("Vascr LM", {
  expect_snapshot(vascr_lm(growth.df, "R", 4000, 100))
  
})

test_that("Vascr_residuals", {
  expect_snapshot(vascr_residuals(growth.df, "R", "4000", 100))
})

test_that("Vascr Shapiro test checks", {
  expect_snapshot(vascr_shapiro(growth.df, "R", 4000, 100))
})

test_that("Levene test", {
  expect_snapshot(vascr_levene(growth.df, "R", 4000, 100))
})

test_that("QQ plot", {
  testthat::skip_on_ci()
  vdiffr::expect_doppelganger("QQ plot", vascr_plot_qq(growth.df, "R", 4000, 100) + labs(title = "NONE"))
})

test_that("Normality plot", {
  testthat::skip_on_ci()
  vdiffr::expect_doppelganger("normality plot", vascr_plot_normality(growth.df, "R", 4000, 100))
})

test_that("Levene plot", {
  testthat::skip_on_ci()
  # vdiffr::expect_doppelganger("Levene plot", vascr_plot_levene(growth.df, "R", 4000, 100))
})


test_that("Plot Time Vline", {
  testthat::skip_on_ci()
  vdiffr::expect_doppelganger("time vline", vascr_plot_time_vline(growth.df, "R", 4000, 100))
})


test_that("Box plot replicate", {
  testthat::skip_on_ci()
  vdiffr::expect_doppelganger("Box plot replicate", vascr_plot_box_replicate(growth.df, "R", 4000, 100))
})


test_that("Tukey tests", {
  testthat::skip_on_ci()
  expect_snapshot(vascr_tukey(growth.df, "R", 4000, 100))
  expect_snapshot(vascr_tukey(growth.df, "R", 4000, 100, raw = TRUE))
})

test_that("Dunnett test works", {
  testthat::skip_on_ci()
  #one time
  expect_snapshot(vascr_dunnett(growth.df, "R", 4000, 50, 8))
  #Two times
  expect_snapshot(vascr_dunnett(growth.df, "R", 4000, list(50, 100), 8))
  # With name rather than ID
  expect_snapshot(vascr_dunnett(growth.df, "R", 4000, 50, "0_cells + HCMEC D3_line"))
})


test_that("Dunnett test works", {
  testthat::skip_on_ci()
  
   suppressMessages({vdiffr::expect_doppelganger("Dunnett 1", vascr_plot_line_dunnett(growth.df, unit = "R", frequency = 4000, time = 25, reference = "0_cells + HCMEC D3_line"))  })
  # vdiffr::expect_doppelganger("Dunnett 2",vascr_plot_line_dunnett(growth.df, unit = "R", frequency = 4000, time = list(25,100), reference = "0_cells + HCMEC D3_line"))
  # vdiffr::expect_doppelganger("Dunnett 3",vascr_plot_line_dunnett(growth.df, unit = "R", frequency = 4000, time = 180, reference = "20,000_cells + HCMEC D3_line"))
 
})

test_that("Anova bar against reference", {
  testthat::skip_on_ci()
  suppressWarnings({
  vdiffr::expect_doppelganger("Anova reference", vascr_plot_bar_dunnett(growth.df, "R", 4000, 50, 8, stars =TRUE))
  vdiffr::expect_doppelganger("Anova reference 2 dunnett", vascr_plot_bar_dunnett(growth.df %>% vascr_subset(sampleid = c(2,3,8)), "R", 4000, 50, 8, stars =FALSE))
  })
  })

test_that("Plot bar anova", {
  testthat::skip_on_ci()
  vdiffr::expect_doppelganger("anova bar", vascr_plot_bar_anova(data = growth.df, confidence = 0.95, unit = "R", time = 100, frequency = 4000, rotate_x_angle = 45))
})

test_that("Plot overall ANOVA tabulation", {
  testthat::expect_warning(vascr_plot_anova(data.df = growth.df, unit = "R", frequency = 4000, time = 100))
  testthat::skip_on_ci()
  vdiffr::expect_doppelganger("Overall anova plot", vascr_plot_anova(data.df = growth.df, unit = "R", frequency = 4000, time = 100))
  set.seed(100)
  invisible(vdiffr::expect_doppelganger("anova bar 2", {vascr_plot_anova(data = growth.df, unit = "R", time = 110, frequency = 4000, reference = "0_cells + HCMEC D3_line")}))
  
  
  nines = vascr_combine(growth.df %>% mutate(Experiment = paste(Experiment, "1")), growth.df %>% mutate(Experiment = paste(Experiment, "2")), growth.df %>% mutate(Experiment = paste(Experiment, "3")))
  vdiffr::expect_doppelganger("Overall anova plot 2, nine reps", vascr_plot_anova(data.df = nines, unit = "R", frequency = 4000, time = 100))
  
})


test_that("Can plot out the anova graphics", {
  testthat::skip_on_ci()
  vdiffr::expect_doppelganger("overall plot", vascr_plot_anova_grid(growth.df, "R", 4000, 100))
})




