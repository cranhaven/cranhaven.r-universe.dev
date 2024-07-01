test_that("fcwt() returns a vector of expected length", {
  expect_length(
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1
    ),
    1000 * 10
  )

  expect_length(
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 10
    ),
    1000 * 10
  )
})

test_that("fcwt() optional arguments do work", {
  expect_no_error(
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 44100,
      n_freqs = 10
    )
  )
})

test_that("fcwt() errs if frequency specs are higher than Nyquist frequency", {
  expect_error(
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 25000,
      n_freqs = 10,
      sigma = 1
    )
  )
})
