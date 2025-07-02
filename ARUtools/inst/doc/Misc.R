## ----include = FALSE----------------------------------------------------------
library(ARUtools)
library(dplyr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----eval=F-------------------------------------------------------------------
# site_list <-
#   example_sites |>
#   tidyr::separate(Sites, into = c("plot", "site"), sep = "_", remove = F) |>
#   dplyr::select(site_id = Sites, plot, site)
# 
# 
# tmp_dir <- tempdir(check = T) |> paste0("/ARUtools/")
# dir.create(tmp_dir)
# 
# create_directory_structure(
#   hexagons = site_list$plot,
#   units = site_list$site_id,
#   base_dir = tmp_dir
# )

## ----eval=F-------------------------------------------------------------------
# list.dirs(tmp_dir, full.names = F)

## -----------------------------------------------------------------------------
wind_files <-
  wind_detection_pre_processing(
    wav_files = example_clean$path,
    output_directory = "./wind_files/",
    site_pattern = create_pattern_site_id(
      p_digits = c(2, 3), sep = "_",
      s_digits = c(1, 2)
    ),
    write_to_file = F, chunk_size = NULL
  )

## -----------------------------------------------------------------------------
example_json <- system.file("extdata", "P71-1__20210606T232500-0400_SS.json", package = "ARUtools")

wind_summary <- wind_detection_summarize_json(example_json)
dplyr::glimpse(wind_summary)

## ----eval=F-------------------------------------------------------------------
# in_tasks <- fs::file_temp("Input_task_file", ext = ".csv")
# task_template <- wildRtrax::wt_make_aru_tasks(
#   example_clean |>
#     dplyr::mutate(
#       recording_date_time = date_time,
#       file_path = path, location = site_id,
#       length_seconds = 300
#     ),
#   output = in_tasks,
#   task_method = "1SPT", task_length = 300
# )

## -----------------------------------------------------------------------------
template_observers

## -----------------------------------------------------------------------------
task_output <- wt_assign_tasks(
  wt_task_template_in = task_template,
  wt_task_output_file = NULL,
  interp_hours = template_observers,
  interp_hours_column = hrs,
  random_seed = 65416
)

task_output$task_summary

## ----eval=F-------------------------------------------------------------------
# shiny::runGitHub("dhope/Shiny_select")

