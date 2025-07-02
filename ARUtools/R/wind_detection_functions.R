#' Summarize wind detection results
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function takes output from the command line program and summarizes it.
#' Details of the wind detection software can be found at
#' [https://github.com/dhope/WindNoiseDetection](https://github.com/dhope/WindNoiseDetection).
#'
#'
#' @param f filepath for json
#'
#'#'
#' @return tibble of summarized data from json file
#'
#' @export
#'
#' @examples
#' # example code
#'
#'  example_json <- system.file("extdata",
#'  "P71-1__20210606T232500-0400_SS.json",
#'  package = "ARUtools"
#'  )
#'
#'  wind_summary <- wind_detection_summarize_json(example_json)
#'
wind_detection_summarize_json <- function(f) {
  lifecycle::signal_stage("experimental", "ARUtools::wind_detection_summarize_json()")
  check_installed("jsonlite",reason = "sum_json requires {jsonlite} package")

  s <- purrr::safely(jsonlite::read_json)

  jsonfile <- s(f)
  jsonFilename <-  fs::path_file(f)
  if (is_null(jsonfile$result)) {
    return(dplyr::tibble(
      jsonF = jsonFilename
    ))
  } else {
    jsonfile <- jsonfile$result
  }
  nm <- purrr::pluck(jsonfile, "FileName")
  dets <- purrr::pluck(jsonfile, "Time History") |>
    purrr::transpose() |>
    purrr::pluck("Te") |>
    purrr::list_c() |>
    max()
  if (is_empty(jsonfile$`Wind free regions`)) {
    return(dplyr::tibble(
      path = nm, totalwindless = 0,
      length = dets,
      pwindless = 0,
      n = 0, mean_windless = 0,
      jsonF = jsonFilename
    ))
  }

  tmp <- purrr::pluck(jsonfile, "Wind free regions") |> purrr::transpose()
  nm <- purrr::pluck(jsonfile, "FileName")
  e <- tmp |>
    purrr::pluck("e") |>
    purrr::list_c()
  s <- purrr::list_c(purrr::pluck(tmp, "s"))

  dplyr::tibble(s, e) |>
    dplyr::mutate(t = e - s) |>
    dplyr::summarize(
      totalwindless = sum(t),
      pwindless = totalwindless / dets,
      n = dplyr::n(),
      length = dets,
      mean_windless = mean(t),
      path = nm,
      jsonF = jsonFilename
    )
}



#' Pre-processing of files for Wind Detection program
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function takes a vector of wave file names and returns a list
#'  of three vectors that can be provided to the wind detection software or
#'  written to files that the software can read. Details of the usable fork of the
#'  wind detection software can be found at
#' [https://github.com/dhope/WindNoiseDetection](https://github.com/dhope/WindNoiseDetection)
#'
#' @param wav_files Vector of path to wav files
#' @param site_pattern Pattern to extract sites from file names
#' @param output_directory Directory path to export files to
#' @param write_to_file Logical Should the function write files to output_directory
#' @param chunk_size Numeric If not NULL, sets number of files to include in each chunk
#'
#' @return List including filePath, filenames, and sites suitable for wind software.
#' @export
#'
#' @examples
#'  wind_files <-
#'  wind_detection_pre_processing(
#'  wav_files = example_clean$path,
#'    output_directory = td,
#'      site_pattern = create_pattern_site_id(
#'          p_digits = c(2, 3), sep = "_",
#'              s_digits = c(1, 2)
#'                ),
#'                  write_to_file = FALSE, chunk_size = NULL
#'                  )
#'
#'
wind_detection_pre_processing <- function(wav_files, site_pattern, output_directory, write_to_file = FALSE,
                                             chunk_size = NULL) {
  lifecycle::signal_stage("experimental", "ARUtools::wind_detection_pre_processing()")
  if (any(grepl("[\\(,\\),\\+,\\[,\\]", wav_files))) {
    warn(
      c("Special characters detected in file paths",
        x = "Wind Detection is may not work"
      )
    )
  }
  gen_output <- function(file_list) {
    filePaths <- fs::path_dir(file_list)

    sites <- file_list |>
      stringr::str_extract(site_pattern)

    filenames <- fs::path_file(file_list) |>
      fs::path_ext_remove()

    list(
      filePaths = filePaths,
      filenames = filenames,
      sites = sites
    )
  }

  if (is.null(chunk_size)) {
    output <- gen_output(wav_files)
    if (isTRUE(write_to_file)) {
      readr::write_lines(output$filePaths, glue::glue("{output_directory}/pathlist.txt"))
      readr::write_lines(output$filenames, glue::glue("{output_directory}/filelist.txt"))
      readr::write_lines(output$sites, glue::glue("{output_directory}/sitelist.txt"))
    }
  } else {
    check_installed("parallel", "Using the chunks command requires installation of the 'parallel' package.
                    Set chunks = NULL or install the package")
    chunks <- parallel::splitIndices(length(wav_files), ncl = ceiling(length(wav_files) / (chunk_size)))
    output <- purrr::map(chunks, ~ gen_output(wav_files[.x]))

    if (isTRUE(write_to_file)) {
      dir_ <- purrr::walk(
        1:length(chunks),
        ~ {
          dir.create(glue::glue("{output_directory}/{.x}"))
          readr::write_lines(output[[.x]]$filePaths, here::here(glue::glue("{output_directory}/{.x}/pathlist.txt")))
          readr::write_lines(output[[.x]]$filenames, here::here(glue::glue("{output_directory}/{.x}/filelist.txt")))
          readr::write_lines(output[[.x]]$sites, here::here(glue::glue("{output_directory}/{.x}/sitelist.txt")))
        }
      )
    }
  }
  return(output)
}

