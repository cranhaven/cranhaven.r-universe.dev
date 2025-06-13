library(lifelogr)
context("Person object creation")


test_that("can create Person object with other info fields", {
  expect_is(Person$new(user_info = list("name" = "NN", "workdays" = list("Tues", 
                                                                         "Sat"),
                                        "fav_color" = "purple"),
                       start_date = "2017-03-11", end_date = "2017-03-12"), 
            "Person")
  expect_is(Person$new(user_info = list("name" = "NN", "workdays" = list("Tues", 
                                                                         "Sat"),
                                        "fav_color" = "purple"),
                       start_date = "2017-03-11", 
                       end_date = "2017-03-12")$user_info, 
            "list")
})



test_that("can load in additional data as dataframe", {
  expect_is(Person$new(addl_data = data.frame(NA),
                       start_date = "2017-03-11", end_date = "2017-03-12"), 
            "Person")
  expect_is(Person$new(addl_data = data.frame(NA),
                       start_date = "2017-03-11", 
                       end_date = "2017-03-12")$addl_data, "data.frame")
})



test_that("can load in list of dataframe grouping assignments", {
  expect_is(Person$new(group_assignments = list("group_months" =
                            data.frame("month" = c("Jan", 
                                         "Feb", "Mar", "Apr", 
                                           "May", "Jun", "Jul", "Aug",
                                           "Sep", "Oct", "Nov", "Dec"),
                                           "group" = c(0, 0, 0, 1, 1, 1,
                                                       1, 1, 1, 0, 0, 0)), 
                    data.frame(NA))), "Person")
  
})



test_that("no error when creating Person object with no start and end date", {
  expect_is(Person$new(user_info = list("name" = "NN", "workdays" = list("Tues", 
                                    "Sat"),"fav_color" = "purple")), "Person")
})



test_that("no error when creating empty Person object", {
  expect_is(Person$new(), "Person")
  
})



