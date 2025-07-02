test_that("wind_detection_summarize_json() works", {


  example_json <- system.file("extdata",
    "P71-1__20210606T232500-0400_SS.json",
    package = "ARUtools"
  )

  # Check failures with no file and corrupted file
  expect_warning(wind_summary_no_file <- wind_detection_summarize_json("Not a file"), "cannot open file")
  expect_equal(wind_summary_no_file$jsonF, "Not a file")

  bad_file <- "\\{
                        \"Global Stats\"\\:
          \\[ 89.3, 0.3, 3.0, 3.0, 4.0, 0.3\\],
          "
  tf <- tempfile(fileext = ".json")
  readr::write_lines(bad_file, tf)
  expect_silent(wind_summary_bad_file <- wind_detection_summarize_json(tf))
  expect_equal(wind_summary_bad_file$jsonF, fs::path_file(tf))
  expect_false(any(c(
    "length", "totalwindless",
    "pwindless", "n",
    "mean_windless", "path"
  ) %in%
    c(
      names(wind_summary_no_file),
      names(wind_summary_bad_file)
    )))


  expect_silent(wind_summary <- wind_detection_summarize_json(example_json))


  windy_file <-
    '{
 	"Global Stats":
 	 [
		89.3,	0.3,	3.0,	3.0,	4.0,	0.3
		],
	 "FileName": "/cygdrive/P/Path/To/WaveFile/NL/P71/P71-1/Windy_0400_SS.wav",
	 "Time History":
	 [
          {"Ts": 0.00,"Te": 0.99, "dBA": 66, "QDeg": 61.46},
		{"Ts": 0.99,"Te": 1.99, "dBA": 68, "QDeg": 63.36},
		{"Ts": 1.99,"Te": 2.98, "dBA": 73, "QDeg": 65.42},
		{"Ts": 2.98,"Te": 3.98, "dBA": 73, "QDeg": 63.65},
		{"Ts": 3.98,"Te": 4.98, "dBA": 69, "QDeg": 62.33},
		{"Ts": 4.98,"Te": 5.98, "dBA": 67, "QDeg": 57.12},
		{"Ts": 5.98,"Te": 6.98, "dBA": 68, "QDeg": 50.42}
	],
	"Wind free regions":
 	 [

	]
}
'
  tf2 <- tempfile(fileext = ".json")
  readr::write_lines(windy_file, tf2)
  expect_silent(wind_summary_all_wind <- wind_detection_summarize_json(tf2))
  expect_equal(wind_summary_all_wind$path, "/cygdrive/P/Path/To/WaveFile/NL/P71/P71-1/Windy_0400_SS.wav")
  expect_equal(wind_summary_all_wind$jsonF, fs::path_file(tf2))
  expect_equal(wind_summary_all_wind$totalwindless, 0)
  expect_equal(wind_summary_all_wind$length, 6.98)
  expect_equal(wind_summary_all_wind$pwindless, 0)
  expect_equal(wind_summary_all_wind$n, 0)
  expect_equal(wind_summary_all_wind$mean_windless, 0)
})


test_that("wind_detection_pre_processing()", {
  td <- tempdir()


  wind_files <-
    wind_detection_pre_processing(
      wav_files = example_clean$path,
      output_directory = td,
      site_pattern = create_pattern_site_id(
        p_digits = c(2, 3), sep = "_",
        s_digits = c(1, 2)
      ),
      write_to_file = T, chunk_size = NULL
    )

  expect_contains(list.files(td), c("filelist.txt", "pathlist.txt", "sitelist.txt"))
  expect_equal(wind_files$filePaths, fs::path_dir(example_clean$path))
  expect_equal(wind_files$filenames, fs::path_ext_remove(example_clean$file_name))

  # Check files are equivalent to list data
  expect_equal(wind_files$filePaths, readr::read_lines(glue::glue("{td}/pathlist.txt")))
  expect_equal(wind_files$filenames, readr::read_lines(glue::glue("{td}/filelist.txt")))
  expect_equal(wind_files$sites, readr::read_lines(glue::glue("{td}/sitelist.txt")))

  expect_silent(
  wind_files_chunked <-
    wind_detection_pre_processing(
      wav_files = example_clean$path,
      output_directory = td,
      site_pattern = create_pattern_site_id(
        p_digits = c(2, 3), sep = "_",
        s_digits = c(1, 2)
      ),
      write_to_file = T, chunk_size = 2
    ) )


  expect_equal(suppressWarnings(list.dirs(td, recursive = F, full.names = F) |> as.numeric() |> sort()),
               1:(ceiling(nrow(example_clean)/2)))

  expect_equal(length(wind_files_chunked), ceiling(nrow(example_clean)/2))


  expect_warning(
    wind_files_warn <-
      wind_detection_pre_processing(
        wav_files = "FilesWithBadChar[inhere]",
        output_directory = NULL,
        site_pattern = create_pattern_site_id(
          p_digits = c(2, 3), sep = "_",
          s_digits = c(1, 2)
        ),
        write_to_file = F, chunk_size = NULL
      ), "Special characters detected"

  )







})
