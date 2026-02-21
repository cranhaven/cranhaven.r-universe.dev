


test_that("Can summarise", {
  
  rbgrowth.df = growth.df %>% vascr_subset(unit = "Rb")
  
  expect_snapshot(vascr_summarise(rbgrowth.df, level = "summary"))
		
  expect_snapshot(vascr_summarise(rbgrowth.df, level = "experiments"))
  
  expect_snapshot(vascr_summarise(rbgrowth.df, level = "experiments") %>% vascr_summarise(level = "summary"))
	
  expect_snapshot(vascr_summarise(rbgrowth.df, level = "wells"))
  
  expect_snapshot(vascr_summarise(rbgrowth.df, level = "median_deviation"))
  
  
  expect_snapshot_error(vascr_summarise_experiments(vascr_summarise(rbgrowth.df, "summary")))
  
  expect_snapshot(vascr_summarise_summary(vascr_summarise(rbgrowth.df, "summary")))
  
  rlang::local_options(cli.default_handler = function(msg) invisible(NULL))
  
  # Check un-resampled data
  expect_snapshot({ w16 = system.file('extdata/instruments/ecis_16_testplate.abp', package = 'vascr')
                    d16 = vascr_import("ECIS", raw = w16, experiment = "W16")})
  
  expect_snapshot({vascr_check_resampled(d16)})
  
  expect_snapshot({vascr_summarise(d16 %>% vascr_subset(unit = "R", frequency = 4000), "summary")})
  
})


test_that("Can summarise deviation",{
  expect_snapshot(vascr_summarise(growth.df %>% vascr_subset(unit = "R", frequency = "4000"), level = "median_deviation"))
})

test_that("Can normalise", {
  
  rgrowth.df = growth.df %>% vascr_subset(unit = "R", frequency = 4000, time = c(5,100))
  expect_snapshot(vascr_normalise(data.df = rgrowth.df, 100))
  expect_snapshot(vascr_normalise(rgrowth.df, 100, divide = TRUE))
  
  rgrowth.df = growth.df %>% vascr_subset(unit = "R", frequency = 4000)
  expect_snapshot(vascr_normalise(rgrowth.df, 100))
  
  expect_snapshot(vascr_normalise(growth.df, NULL))

})


test_that("Can subsample", {
  
  expect_snapshot(vascr_subsample(growth.df, 10))
  expect_snapshot(vascr_subsample(growth.df, Inf))
  expect_snapshot(vascr_subsample(growth.df %>% vascr_subset(time = 10), 10))
  expect_snapshot(vascr_resample_time(growth.df, t_start = 5, t_end = 20, rate = 5))
  
})

test_that("Can interpolate time", {
  expect_snapshot(vascr_interpolate_time(growth.df %>% vascr_subset(unit = "Rb")))
  expect_snapshot_error(vascr_interpolate_time(growth.df))
})

test_that("Can interpolate time unresampled dataset", {
  growth_unresampled.df = growth_unresampled.df %>% mutate(Excluded = FALSE)
  expect_snapshot(vascr_resample_time(growth_unresampled.df %>% vascr_subset(unit = "R", frequency = 4000)))
})


test_that("vascr_force_resampled", {
  expect_snapshot(vascr_force_resampled(growth.df))
  growth_unresampled.df$Excluded = "no"
  expect_snapshot(vascr_force_resampled(growth_unresampled.df))
})

test_that("vascr time samples counts correctly", 
{
  expect_snapshot(vascr_find_count_timepoints(growth.df))
})

#vascr auc
test_that("vascr AUC works", 
          {
            expect_snapshot(vascr_auc(growth.df %>% vascr_subset(experiment = 1, well = "A01", unit = "R", frequency = 4000)))
          })


# Plot Resample

test_that("Data can be resampled and plotted",{
            testthat::skip_on_ci()
            vdiffr::expect_doppelganger("vascr_plot_resample raw data", vascr_plot_resample(growth.df))
            vdiffr::expect_doppelganger("vascr_plot_resample raw data 2", vascr_plot_resample(growth.df, plot = TRUE))
          })

test_that("remove metadata",
{
  expect_snapshot(growth.df%>% vascr_remove_metadata())
  expect_snapshot(vascr_summarise(growth.df %>% vascr_subset(unit = "R", frequency = 4000), "experiments") %>% vascr_remove_metadata())
})



test_that("Find resample frequency",{
  vdiffr::expect_doppelganger("resample_frequency_plot", suppressMessages(vascr_find_resample_frequency(growth.df)))
  
})

























































