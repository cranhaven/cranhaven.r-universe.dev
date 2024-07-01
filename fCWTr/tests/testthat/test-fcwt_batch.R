test_that("fcwt batching yields identical result in single-batch case", {
  res0 <-
    fcwt(
      ts_sin_440,
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1
    ) |>
    rm_na_time_slices()

  res_batch <-
    fcwt_batch(
      ts_sin_440,
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1,
      time_resolution = 1 / 44100
    )

  expect_equal(
    res0,
    res_batch
  )
})
