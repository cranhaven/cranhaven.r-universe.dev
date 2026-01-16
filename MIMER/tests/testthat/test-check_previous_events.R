test_that("Check Previous Resistance - base", {

  test_data <- data.frame(subject_id=c('10016742','10016742','10016742','10016742','10016742','10038332','10038332','10038332','10038332','10038332','10038332'),
                          chartdate= c('2178-07-03','2178-08-01','2178-08-01','2178-08-01','2178-09-25','2164-07-31','2164-12-22','2164-12-22','2165-01-07','2165-04-17','2165-05-05'),
                          CEFEPIME=c('R','R','R','R','S','R','R','R','S','S','S'),
                          CEFTAZIDIME=c('S','R','S','R','R','S','S','S','R','R','S'))

  test_df <- test_data %>% check_previous_events(cols = c('CEFEPIME','CEFTAZIDIME'), sort_by_col = 'chartdate', patient_id_col = 'subject_id')


  expected_df <- data.frame(subject_id=c('10038332','10038332','10038332','10038332','10038332','10038332','10016742','10016742','10016742','10016742','10016742'),
                            chartdate= c('2164-07-31','2164-12-22','2164-12-22','2165-01-07','2165-04-17','2165-05-05','2178-07-03','2178-08-01','2178-08-01','2178-08-01','2178-09-25'),
                            CEFEPIME=c('R','R','R','S','S','S','R','R','R','R','S'),
                            CEFTAZIDIME=c('S','S','S','R','R','S','S','R','S','R','R'),
                            pr_event_CEFEPIME =c(FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE),
                            pr_event_CEFTAZIDIME =c(FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE))

  testthat::expect_equal(as.data.frame(test_df),expected_df)
  testthat::expect_equal(dplyr::is_grouped_df(test_df), FALSE)

})


test_that("Check Previous Resistance with 'time_period_in_days' parameter", {

  test_data <- data.frame(subject_id=c('10016742','10016742','10016742','10016742','10016742','10038332','10038332','10038332','10038332','10038332','10038332'),
                          chartdate= c('2178-07-03','2178-08-01','2178-07-22','2178-08-03','2178-09-25','2164-07-31','2164-12-22','2164-12-22','2165-01-07','2165-04-17','2165-05-05'),
                          CEFEPIME=c('R','S','R','S','S','R','R','R','S','S','S'),
                          CEFTAZIDIME=c('S','R','S','R','R','S','S','S','R','R','S'))

  test_df <- test_data %>% check_previous_events(cols = c('CEFEPIME','CEFTAZIDIME'), sort_by_col = 'chartdate', patient_id_col = 'subject_id', time_period_in_days = 25)

  expected_df <- data.frame(subject_id=c('10038332','10038332','10038332','10038332','10038332','10038332','10016742','10016742','10016742','10016742','10016742'),
                            chartdate= c('2164-07-31','2164-12-22','2164-12-22','2165-01-07','2165-04-17','2165-05-05','2178-07-03','2178-07-22','2178-08-01','2178-08-03','2178-09-25'),
                            CEFEPIME=c('R','R','R','S','S','S','R','R','S','S','S'),
                            CEFTAZIDIME=c('S','S','S','R','R','S','S','S','R','R','R'),
                            pr_event_CEFEPIME =c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE),
                            pr_event_CEFTAZIDIME =c(FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE))

  testthat::expect_equal(as.data.frame(test_df),expected_df)
  testthat::expect_equal(dplyr::is_grouped_df(test_df), FALSE)

})

test_that("Check Previous Resistance with 'minimum_prev_events' parameter", {

  test_data <- data.frame(subject_id=c('10016742','10016742','10016742','10016742','10016742','10038332','10038332','10038332','10038332','10038332','10038332'),
                          chartdate= c('2178-07-03','2178-08-01','2178-07-22','2178-08-03','2178-09-25','2164-07-31','2164-12-22','2164-12-22','2165-01-07','2165-04-17','2165-05-05'),
                          CEFEPIME=c('R','S','R','S','S','R','R','R','S','S','S'),
                          CEFTAZIDIME=c('S','R','S','R','R','S','S','S','R','R','S'))

  test_df <- test_data %>% check_previous_events(cols = c('CEFEPIME','CEFTAZIDIME'), sort_by_col = 'chartdate', patient_id_col = 'subject_id', minimum_prev_events = 2)

  expected_df <- data.frame(subject_id=c('10038332','10038332','10038332','10038332','10038332','10038332','10016742','10016742','10016742','10016742','10016742'),
                            chartdate= c('2164-07-31','2164-12-22','2164-12-22','2165-01-07','2165-04-17','2165-05-05','2178-07-03','2178-07-22','2178-08-01','2178-08-03','2178-09-25'),
                            CEFEPIME=c('R','R','R','S','S','S','R','R','S','S','S'),
                            CEFTAZIDIME=c('S','S','S','R','R','S','S','S','R','R','R'),
                            pr_event_CEFEPIME =c(FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE),
                            pr_event_CEFTAZIDIME =c(FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE))

  testthat::expect_equal(as.data.frame(test_df),expected_df)
  testthat::expect_equal(dplyr::is_grouped_df(test_df), FALSE)

})


test_that("Check Previous Resistance with 'minimum_prev_events' + 'time_period_in_days' parameters", {

  test_data <- data.frame(subject_id=c('10016742','10016742','10016742','10016742','10016742','10038332','10038332','10038332','10038332','10038332','10038332'),
                          chartdate= c('2178-07-03','2178-08-01','2178-07-22','2178-08-03','2178-09-25','2164-07-31','2164-12-22','2164-12-22','2165-01-07','2165-04-17','2165-05-05'),
                          CEFEPIME=c('R','S','R','S','S','R','R','R','S','S','S'),
                          CEFTAZIDIME=c('S','R','S','R','R','S','S','S','R','R','S'))

  test_df <- test_data %>% check_previous_events(cols = c('CEFEPIME','CEFTAZIDIME'), sort_by_col = 'chartdate', patient_id_col = 'subject_id', time_period_in_days = 62, minimum_prev_events = 2)

  expected_df <- data.frame(subject_id=c('10038332','10038332','10038332','10038332','10038332','10038332','10016742','10016742','10016742','10016742','10016742'),
                            chartdate= c('2164-07-31','2164-12-22','2164-12-22','2165-01-07','2165-04-17','2165-05-05','2178-07-03','2178-07-22','2178-08-01','2178-08-03','2178-09-25'),
                            CEFEPIME=c('R','R','R','S','S','S','R','R','S','S','S'),
                            CEFTAZIDIME=c('S','S','S','R','R','S','S','S','R','R','R'),
                            pr_event_CEFEPIME =c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE),
                            pr_event_CEFTAZIDIME =c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE))

  testthat::expect_equal(as.data.frame(test_df),expected_df)
  testthat::expect_equal(dplyr::is_grouped_df(test_df), FALSE)

})


test_that("Check Previous Resistance with 'minimum_prev_events' + 'time_period_in_days' parameters with events in same date scenario", {

  test_data <- data.frame(subject_id=c('10016742','10016742','10016742','10016742','10016742','10038332','10038332','10038332','10038332','10038332','10038332'),
                          chartdate= c('2178-07-03','2178-08-01','2178-08-01','2178-08-01','2178-09-25','2164-07-31','2164-12-22','2164-12-22','2165-01-07','2165-04-17','2165-05-05'),
                          CEFEPIME=c('R','R','R','R','S','R','R','R','S','S','S'),
                          CEFTAZIDIME=c('S','R','S','R','R','S','S','S','R','R','S'))

  test_df <- test_data %>% check_previous_events(cols = c('CEFEPIME','CEFTAZIDIME'), sort_by_col = 'chartdate', patient_id_col = 'subject_id', time_period_in_days = 62, minimum_prev_events = 2)

  expected_df <- data.frame(subject_id=c('10038332','10038332','10038332','10038332','10038332','10038332','10016742','10016742','10016742','10016742','10016742'),
                            chartdate= c('2164-07-31','2164-12-22','2164-12-22','2165-01-07','2165-04-17','2165-05-05','2178-07-03','2178-08-01','2178-08-01','2178-08-01','2178-09-25'),
                            CEFEPIME=c('R','R','R','S','S','S','R','R','R','R','S'),
                            CEFTAZIDIME=c('S','S','S','R','R','S','S','R','S','R','R'),
                            pr_event_CEFEPIME =c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE),
                            pr_event_CEFTAZIDIME =c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE))

  testthat::expect_equal(as.data.frame(test_df),expected_df)
  testthat::expect_equal(dplyr::is_grouped_df(test_df), FALSE)

})

test_that("Check Previous Resistance with 'minimum_prev_events' + 'time_period_in_days' parameters with charttime sort_by_column with NA", {

  test_data <- data.frame(subject_id=c('10016742','10016742','10016742','10016742','10016742','10038332','10038332','10038332','10038332','10038332','10038332'),
                          charttime= c('2178-07-03 04:32:00','2178-08-01 11:22:00',NA,'2178-08-03 14:32:00','2178-09-25 11:12:00','2164-07-31 12:02:00','2164-12-22 14:32:00','2164-12-22 03:32:00',NA,'2165-04-17 01:03:00','2165-05-05 12:22:00'),
                          CEFEPIME=c(NA,'S','R','S','S','R','R','R','S','S','S'),
                          CEFTAZIDIME=c('S','R','S','R','R','S','S','S','R','R','S'))

  test_df <- test_data %>% check_previous_events(cols = c('CEFEPIME','CEFTAZIDIME'), sort_by_col = 'charttime', patient_id_col = 'subject_id', time_period_in_days = 62, minimum_prev_events = 2, default_na_date = '9999-12-31 00:00:00')

  expected_df <- data.frame(subject_id=c('10038332','10038332','10038332','10038332','10038332','10016742','10016742','10016742','10016742','10016742','10038332'),
                            charttime= c('2164-07-31 12:02:00','2164-12-22 03:32:00','2164-12-22 14:32:00','2165-04-17 01:03:00','2165-05-05 12:22:00','2178-07-03 04:32:00','2178-08-01 11:22:00','2178-08-03 14:32:00','2178-09-25 11:12:00','9999-12-31 00:00:00','9999-12-31 00:00:00'),
                            CEFEPIME=c('R','R','R','S','S','NA','S','S','S','R','S'),
                            CEFTAZIDIME=c('S','S','S','R','S','S','R','R','R','S','R'),
                            pr_event_CEFEPIME =c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
                            pr_event_CEFTAZIDIME =c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE))

  testthat::expect_equal(as.data.frame(test_df),expected_df)
  testthat::expect_equal(dplyr::is_grouped_df(test_df), FALSE)

})

