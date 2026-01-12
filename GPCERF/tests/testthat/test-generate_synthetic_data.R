test_that("generate_synthetic_data works as expected!", {

  set.seed(298)
  mydata <- generate_synthetic_data(sample_size = 200)

  expect_equal(class(mydata), "data.frame")
  expect_equal(nrow(mydata), 200L)
  expect_equal(length(mydata), 8L)
  expect_equal(mydata[10, 2], 9.771912, tolerance = 0.00001)


  set.seed(256)
  mydata <- generate_synthetic_data(sample_size = 300, outcome_sd = 10,
                                    gps_spec = 2, cova_spec = 2)

  expect_equal(class(mydata), "data.frame")
  expect_equal(nrow(mydata), 300L)
  expect_equal(length(mydata), 8L)
  expect_equal(mydata[15, 3], 1.229907, tolerance = 0.00001)

  set.seed(811)
  mydata <- generate_synthetic_data(sample_size = 300, outcome_sd = 10,
                                    gps_spec = 3, cova_spec = 2)

  expect_equal(class(mydata), "data.frame")
  expect_equal(nrow(mydata), 300L)
  expect_equal(length(mydata), 8L)
  expect_equal(mydata[20, 1], 15.20656, tolerance = 0.00001)


  set.seed(769)
  mydata <- generate_synthetic_data(sample_size = 400, outcome_sd = 10,
                                    gps_spec = 4, cova_spec = 2)

  expect_equal(class(mydata), "data.frame")
  expect_equal(nrow(mydata), 400L)
  expect_equal(length(mydata), 8L)
  expect_equal(mydata[25, 1], 25.30991, tolerance = 0.00001)

  set.seed(769)
  mydata <- generate_synthetic_data(sample_size = 200, outcome_sd = 10,
                                    gps_spec = 5, cova_spec = 2)

  expect_equal(class(mydata), "data.frame")
  expect_equal(nrow(mydata), 200L)
  expect_equal(length(mydata), 8L)
  expect_equal(mydata[14, 1], 3.584463, tolerance = 0.00001)

  set.seed(987)
  mydata <- generate_synthetic_data(sample_size = 200, outcome_sd = 10,
                                    gps_spec = 6, cova_spec = 2)

  expect_equal(class(mydata), "data.frame")
  expect_equal(nrow(mydata), 200L)
  expect_equal(length(mydata), 8L)
  expect_equal(mydata[69, 1], 99.96737, tolerance = 0.00001)


})
