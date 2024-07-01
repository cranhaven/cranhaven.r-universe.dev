test_that("tbind() for fcwtr_scalogram", {
  first <-
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1
    )

  second <-
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1
    )

  third <-
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 22100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1
    )

  expect_s3_class(
    first,
    "fcwtr_scalogram"
  )
  expect_s3_class(
    second,
    "fcwtr_scalogram"
  )
  expect_s3_class(
    tbind(first, second),
    "fcwtr_scalogram"
  )
  expect_error(
    tbind(first, third)
  )
})

test_that("ggplot2::autoplot() does not err", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")

  res <-
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1
    )

  expect_no_error(ggplot2::autoplot(res))
  expect_s3_class(ggplot2::autoplot(res), "ggplot")
})

test_that("as.data.frame() `time_ind` columns is reasonable", {
  time <-
    as.data.frame(
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1
    )
  )[["time"]]

  expect_gte(min(time), 0)
  expect_lte(max(time), 1)
})
