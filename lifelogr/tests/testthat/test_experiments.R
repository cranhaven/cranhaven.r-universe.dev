library(lifelogr)
context("Output of Experiments")

test_that("experiment with nonexistant analysis produces correct error", {
  expect_error(experiment(person = EX, variables = list("fitbit_daily" = c("sleepDuration"), 
                                                        "util" = c("day_of_week")),
                          measures = list("fitbit_daily" = c("restingHeartRate")), 
                          analysis = c("kriging"),
                          time_var = c("date")), 
               'your type of analysis did not match an available options')
})

test_that("experiment with wrong time_var produces correct error", {
  expect_error(experiment(person = EX, variables = list("fitbit_daily" = c("sleepDuration"), 
                                                        "util" = c("day_of_week")),
                          measures = list("fitbit_daily" = c("restingHeartRate")), 
                          analysis = c("plot"),
                          time_var = c("lunar")), 
               "time_var must be 'time', 'date', or 'datetime'")
})

test_that("create_datasets with wrong time_var produces correct error", {
  expect_error(dataset <- create_dataset(person = EX,
                              all_variables = list("util" = c("month"),
                                                   "fitbit_daily" = c("steps")), 
                              time_var = c("lunar")), 
               "time_var must be 'time', 'date', or 'datetime'")
})

test_that("create_datasets with NA time_var produces correct error", {
  expect_error(dataset <- create_dataset(person = EX,
                                         all_variables = list("util" = c("month"),
                                                              "fitbit_daily" = c("steps")), 
                                         time_var = NA),
  "time_var must be 'time', 'date', or 'datetime'")
})


test_that("correctly merge lists where each list has one, and the same, source", {
  expect_equal(names(merge_lists(list(list("fitbit_daily" = c("distance")),
                                list("fitbit_daily" = c("steps"))))),
               "fitbit_daily")
  
})


test_that("correctly merge lists where each list has multiple, 
          and different, sources", {
    expect_equal(names(merge_lists(list(list("fitbit_daily" = c("distance"), 
                                             "util" = c("day_of_week")),
          list("fitbit_daily" = c("steps"), 
               "fitbit_intraday" = c("steps"))))), c("fitbit_daily",
                                                     "util", "fitbit_intraday"))
            
  
})

