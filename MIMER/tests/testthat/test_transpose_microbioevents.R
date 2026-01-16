test_that("Check Transpose Functionality", {

  test_data <- data.frame(subject_id=c('10016742','10016742','10016742','10016742','10016742','10038332','10038332','10038332','10038332','10038332','10038332'),
                          chartdate= c('2178-07-03','2178-08-01','2178-08-01','2178-08-01','2178-09-25','2164-07-31','2164-12-22','2164-12-22','2165-01-07','2165-04-17','2165-05-05'),
                          ab_name=c('CEFEPIME','CEFTAZIDIME','CEFEPIME','CEFEPIME','CEFTAZIDIME','CEFTAZIDIME','CEFEPIME','CEFEPIME','CEFTAZIDIME','CEFTAZIDIME','CEFEPIME'),
                          interpretation=c('S','R','S','R','R','S','S','S','R','R','S'))

  test_df <- test_data %>% transpose_microbioevents(key_columns = c('subject_id','chartdate','ab_name') , required_columns =c('subject_id','chartdate'), transpose_key_column = 'ab_name',
                                                    transpose_value_column = 'interpretation', fill = "N/A", non_empty_filter_column ='subject_id')


  expected_df <- data.frame(subject_id=c('10016742','10016742','10016742','10038332','10038332','10038332','10038332'),
                            chartdate= c('2178-07-03','2178-08-01','2178-09-25','2164-07-31','2165-01-07','2165-04-17','2165-05-05'),
                            CEFEPIME=c('S','N/A','N/A','N/A','N/A','N/A','S'),
                            CEFTAZIDIME=c('N/A','R','R','S','R','R','N/A'))

  testthat::expect_equal(as.data.frame(test_df),expected_df)

})
