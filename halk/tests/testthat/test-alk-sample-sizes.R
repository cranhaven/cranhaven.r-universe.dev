test_that("make_alk correctly creates ALK when supposed to", {
  expect_type(make_alk(laa_data), "list")
  expect_warning(make_alk(laa_data, min_total_sample_size = 1000))
  expect_warning(make_alk(laa_data_low_n, min_total_sample_size = 30))
  expect_type(make_alk(laa_data_low_n, min_total_sample_size = 20), "list")
  expect_warning(
    make_alk(laa_data_low_age_n, min_age_sample_size = 10, min_age_groups = 4)
  )
  expect_type(make_alk(laa_data_low_age_n, min_age_sample_size = 1), "list")
  expect_warning(make_alk(laa_data_few_ages))
  expect_type(make_alk(laa_data_few_ages, min_age_groups = 2), "list")
})

test_that("make_halk correctly creates ALK for species when supposed to", {
  expect_type(make_halk(spp_data, levels = "spp"), "list")
  expect_warning(
    make_halk(spp_data, levels = "spp", min_total_sample_size = 1000)
  )
  expect_warning(
    make_halk(spp_data_low_n, levels = "spp", min_total_sample_size = 30)
  )
  expect_type(
    make_halk(
      spp_data_low_n, levels = "spp",
      min_age_groups = 3,
      min_total_sample_size = 20
    ),
    "list"
  )
  expect_warning(
    make_halk(spp_data_low_age_n, levels = "spp", min_age_groups = 6)
  )
  expect_type(
    make_halk(spp_data_low_age_n, levels = "spp", min_age_sample_size = 1),
    "list"
  )
  expect_warning(make_halk(spp_data_few_ages, levels = "spp"))
  expect_type(
    make_halk(spp_data_few_ages, levels = "spp", min_age_groups = 2),
    "list"
  )
})
