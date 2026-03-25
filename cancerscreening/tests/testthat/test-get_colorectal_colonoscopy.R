test_that("get_colorectal_colonoscopy works", {
  skip_if_no_cred()
  skip_if_offline()

  expect_error(get_colorectal_colonoscopy(), "start_date")
  expect_error(get_colorectal_colonoscopy(start_date = '1234'), 'start_date')
  expect_error(get_colorectal_colonoscopy(start_date = '2022-01-01', end_date = '1234'), 'end_date')
  expect_error(get_colorectal_colonoscopy(start_date = '2020-01-01', level = 'other'), "level")
  expect_no_error(get_colorectal_colonoscopy(start_date = '2023-07-01'))

})
