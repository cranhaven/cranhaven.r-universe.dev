test_that("get_wav_length", {
  f <- tempfile()
  w <- tuneR::sine(440, duration = 100000)
  tuneR::writeWave(w, f)

  expect_silent(x <- get_wav_length(f))
  expect_equal(x, "2.27 seconds")
  expect_silent(x <- get_wav_length(f, return_numeric = TRUE))
  expect_equal(x, 2.27)
  unlink(f)
})

test_that("check_wave_path_in()", {
  f <- c("test.wav", "/home/user/test.wav")
  expect_error(
    check_wave_path_in(c("test.wav", "/home/user/test.wav")),
    "All wave file paths must be either absolute or relative"
  )
  expect_warning(
    check_wave_path_in(c("/test.wav", "/test2.wav"), dir_in = "."),
    "All wave file paths are absolute"
  ) |>
    expect_error("Some wave files could not be found")


  t <- list.files(test_path(), ".R") # grab tests files as examples
  expect_error(
    check_wave_path_in(t, dir_in = test_path()),
    "Non-wav file found in files"
  )
})

test_that("check_wave_path_out()", {
  expect_error(
    check_wave_path_out(
      subdirs = list("non_existant_dir"), filename_out = ".",
      dir_out = ".", create_dir = FALSE
    ),
    "Not all output directories exist"
  )
})

test_that("check_wave_length()", {
  f <- temp_wavs(2)

  expect_error(
    check_wave_length(
      path_in = f, clip_length = 10,
      start_time = 0, diff_limit = 0.5
    ),
    "Some wave files are >=0.5s shorter than the requested clip"
  )
  unlink(f)
})

test_that("clip_wave_single()", {
  # Create a dummy wave file for testing
  f <- temp_wavs(2)

  expect_error(clip_wave_single(f), "More than one file supplied")
  f <- f[1]
  expect_error(
    clip_wave_single(f, path_out = c("test", "test2")),
    "Can only have one output file"
  )

  expect_silent(clip_wave_single(f, test_path("temp.wav"), clip_length = 1, start_time = 0.5))
  expect_error(
    clip_wave_single(f, test_path("temp.wav"), clip_length = 1, start_time = 0.5),
    "`overwrite` is FALSE"
  )
  expect_equal(get_wav_length(test_path("temp.wav"), TRUE), 1)

  # Overwrite respected
  expect_silent(clip_wave_single(f, test_path("temp.wav"),
    clip_length = 0.5, start_time = 0.5,
    overwrite = TRUE
  ))
  expect_equal(get_wav_length(test_path("temp.wav"), TRUE), 0.5)

  # No clipping if clip_length > wave_length
  # Trust wave_length (even if doesn't match)
  expect_silent(clip_wave_single(f, test_path("temp.wav"),
    clip_length = 1.5,
    wave_length = 1, overwrite = TRUE
  ))
  expect_equal(
    get_wav_length(test_path("temp.wav"), TRUE),
    get_wav_length(f, TRUE)
  )

  # Clean up
  unlink(f)
  unlink(test_path("temp.wav"))
})

test_that("clip_wave()", {
  # Create dummy wave files for testing
  w <- data.frame(
    path = temp_wavs(),
    treat = c("test1/a", "test2/a", "test1/b", "test2/b", "test1/c", "test2/c"),
    filename_out = paste0("wave", 1:6, ".wav"),
    clip_length = c(1, 1, 2, 1.5, 0.5, 0.75),
    start_time = c(1.2, 0.5, 1, 0, 0.5, 0.3)
  )

  # Require correct inputs
  expect_error(clip_wave(w), "Require an output directory")
  expect_error(
    clip_wave(w, dir_out = test_path("clean")),
    "Column 'subdir_out' does not exist"
  )

  expect_silent(clip_wave(w, dir_out = test_path("clean"), col_subdir_out = treat))
  expect_true(fs::file_exists(test_path("clean/test1/a/wave1.wav")))

  # Check clip lengths
  expect_equal(get_wav_length(test_path("clean/test1/a/wave1.wav"), TRUE), w$clip_length[1])
  expect_equal(get_wav_length(test_path("clean/test2/a/wave2.wav"), TRUE), w$clip_length[2])

  # Max clip length possible, but less than 30s so no error
  expect_equal(get_wav_length(test_path("clean/test1/b/wave3.wav"), TRUE), get_wav_length(w$path[3], TRUE) - 1)
  expect_equal(get_wav_length(test_path("clean/test2/b/wave4.wav"), TRUE), w$clip_length[4])
  expect_equal(get_wav_length(test_path("clean/test1/c/wave5.wav"), TRUE), w$clip_length[5])
  expect_equal(get_wav_length(test_path("clean/test2/c/wave6.wav"), TRUE), w$clip_length[6])

  # Clean up
  unlink(temp_wavs())
  unlink(test_path("clean"), recursive = TRUE) # Remove this new 'clean' directory
})

test_that("clip_wave() column names", {
  # Create dummy wave files for testing
  w <- data.frame(
    wave_path = temp_wavs(),
    subdir_out = c("test1/a", "test2/a", "test1/b", "test2/b", "test1/c", "test2/c"),
    new_file = paste0("wave", 1:6, ".wav"),
    sample = c(1, 1, 2, 1.5, 0.5, 0.75),
    start = c(1.2, 0.5, 1, 0, 0.5, 0.3)
  )

  expect_silent(clip_wave(w,
    dir_out = test_path("clean"),
    col_path_in = wave_path,
    col_clip_length = sample,
    col_start_time = start,
    col_filename_out = new_file,
    col_subdir_out = subdir_out
  ))
  expect_true(fs::file_exists(test_path("clean/test1/a/wave1.wav")))


  # Clean up
  unlink(temp_wavs())
  unlink(test_path("clean"), recursive = TRUE) # Remove this new 'clean' directory
})

test_that("clip_wave() diff_limit", {
  # Create dummy wave files for testing
  w <- data.frame(
    path = temp_wavs(),
    subdir_out = c("test1", "test2", "test1", "test2", "test1", "test2"),
    filename_out = paste0("wave", 1:6, ".wav"),
    clip_length = c(1, 1, 2, 1.5, 0.5, 0.75),
    start_time = c(1.2, 0.5, 1, 0, 0.5, 0.3)
  )

  expect_error(
    clip_wave(w, dir_out = test_path("clean"), diff_limit = 0.5),
    "Some wave files are >=0.5s shorter than the requested clip"
  )
  expect_false(fs::file_exists(test_path("clean/test1/file1.wav")))

  # clip_wave(w, dir_out = test_path("clean"), use_job = TRUE)
  # job::job({clip_wave(w, dir_out = test_path("clean"))})

  # Clean up
  unlink(temp_wavs())
  unlink(test_path("clean"), recursive = TRUE) # Remove this new 'clean' directory
})


test_that("sox_spectro()", {
  expect_error(sox_spectro("test_wave.wav"), "Cannot find wave file")

  # Prep sample file
  w <- tuneR::sine(440, duration = 300000)
  t <- test_path("test_wave.wav")
  d <- test_path("Spectrograms")
  tuneR::writeWave(w, t)

  # Create spectrograms
  expect_message(sox_spectro(t, dir_out = d), "Writing spectrogram")
  expect_true(fs::file_exists(fs::path(d, "spectro_test_wave.png")))
  expect_silent(sox_spectro(t, dir_out = d, rate = NULL, quiet = TRUE))
  expect_message(sox_spectro(t, dir_out = d, start = 2, end = 3))
  expect_message(sox_spectro(t, dir_out = d, start = "0:01", end = "0:04"))
  expect_message(sox_spectro(t, dir_out = d, prepend = ""))
  expect_true(fs::file_exists(fs::path(d, "test_wave.png")))
  expect_message(sox_spectro(t, dry_run = TRUE), "-n  rate 20k spectrogram -o")

  # Clean up
  unlink(test_path("test_wave.wav"))
  unlink(test_path("Spectrograms"), recursive = TRUE)
})
