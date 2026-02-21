
test_that("Can combine dataframes",{

experiment1.df = vascr_subset(growth.df, experiment = 1)
experiment2.df = vascr_subset(growth.df, experiment = 2)
experiment3.df = vascr_subset(growth.df, experiment = 3)

data2 = vascr_combine(experiment1.df, experiment2.df)
expect_snapshot(data2)

data3 = vascr_combine(experiment1.df, experiment2.df, experiment3.df)
expect_snapshot(data3)

experiment1r.df = vascr_subset(growth.df, experiment = "1 : Experiment 1") %>% vascr_resample_time(npoints = 10)
experiment2r.df = vascr_subset(growth.df, experiment = "2 : Experiment2") %>% vascr_resample_time(npoints = 5)

# Tests with non-re sampled data sets
expect_snapshot(vascr_combine(experiment1r.df, experiment2r.df, experiment3.df))
expect_snapshot(vascr_combine(experiment1r.df, experiment2r.df, experiment3.df) %>% vascr_check_resampled())

expect_snapshot(vascr_combine(experiment1r.df, experiment2r.df, experiment3.df, resample = TRUE))

})
