
library(calcUnique)


#create a random sample of dates to test on
ts_sample <-
  sample(
    as.character(
      seq(from = as.POSIXct('2020-03-01'), to = as.POSIXct('2020-03-15'), by = 'day')
    ),
    size = 30, replace = TRUE
  )


#just the examples from the documentation ... I can't think of others
expect_equal(calcUnique(ts_sample, as.POSIXct), as.POSIXct(ts_sample))

expect_equal(calcUnique(ts_sample, function(i) gsub("00","$$", i)), gsub("00","$$", ts_sample))
