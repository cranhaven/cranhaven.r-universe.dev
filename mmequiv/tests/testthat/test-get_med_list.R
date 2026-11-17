with_mock_dir("get_med_list", {
  test_that("Returns data.frame", {
    med_list <- get_med_list()
    expect_true(is.data.frame(med_list))
  })

  test_that("Able to retrieve all 29 medications", {
    med_list <- get_med_list()
    expect_length(med_list$med_name, 29)
    expect_length(med_list$cf, 29)
  })
})
