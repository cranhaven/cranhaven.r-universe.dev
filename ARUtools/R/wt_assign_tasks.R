#' Assign tasks for interpretation on Wildtrax
#'
#' @param wt_task_template_in Path to csv template downloaded from Wildtrax
#'     platform <https://wildtrax.ca> listing all tasks. Alternatively,
#'     can be a data.frame that is correctly formatted using
#'     `wildRtrax::wt_make_aru_tasks()`. See `vignette("Misc")` for details.
#' @param interp_hours Path to number of hours for each interpreter or a data.table. If a file, must be csv and must include
#'     the columns "transcriber" and whatever the variable `interp_hours_column` is.
#' @param wt_task_output_file Path to csv of output file for uploading to Wildtrax. If left as NULL will not write file
#' @param interp_hours_column LazyEval column name with hours for interpreters
#' @param random_seed Integer. Random seed to select with. If left NULL will use timestamp
#'
#' @return Returns a list with a tibble of assigned tasks and a summary tibble.
#' @export
#'
#' @examples
#'   task_output <- wt_assign_tasks(
#'   wt_task_template_in = task_template,
#'   wt_task_output_file = NULL,
#'   interp_hours = template_observers,
#'   interp_hours_column = hrs,
#'   random_seed = 65122
#'   )
#'
wt_assign_tasks <- function(wt_task_template_in, interp_hours, wt_task_output_file,
                            interp_hours_column, random_seed = NULL) {
  if (is.data.frame(wt_task_template_in)) {
    tasks <- wt_task_template_in
  } else if (fs::file_exists(wt_task_template_in)) {
    if (fs::path_ext(wt_task_template_in) != "csv") {
      abort("'wt_task_template_in' must be a path to a csv file or data table")
    }

    tasks <- readr::read_csv(wt_task_template_in,
      col_types = glue::glue_collapse(rep("c", 13)),
      na = character()
    )
  } else {
    abort("'wt_task_template_in' requires a data.frame or path to csv file")
  }

  if (is.data.frame(interp_hours)) {
    hours <- interp_hours
  } else if (fs::file_exists(interp_hours)) {
    if (fs::path_ext(interp_hours) != "csv") {
      abort("'interp_hours' must be a path to a csv file or data table")
    }
    hours <- readr::read_csv(interp_hours, col_types = readr::cols())
  } else {
    abort("'interp_hours' requires a data.frame or path to a csv file")
  }

  hours <- hours |>
    dplyr::filter(!is.na({{ interp_hours_column }})) |>
    dplyr::mutate(phrs = {{ interp_hours_column }} / sum({{ interp_hours_column }}))

  run_with_seed_if_provided(random_seed, {
    assigned_tasks <- tasks |>
      dplyr::mutate(transcriber = sample(hours$transcriber,
        size = dplyr::n(),
        replace = T, prob = hours$phrs
      ))
  })

  if (!is_null(wt_task_output_file)) {
    readr::write_csv(x = assigned_tasks, file = wt_task_output_file)
  }

  task_summary <-
    dplyr::summarize(
      assigned_tasks,
      hrs_assigned = sum(as.numeric(taskLength)) / 60 / 60,
      .by = transcriber
    ) |>
    dplyr::left_join(hours, by = dplyr::join_by(transcriber)) |>
    dplyr::mutate(updated_hrs_remain = {{ interp_hours_column }} - hrs_assigned) #|> gt::gt()

  return(list(
    assigned_tasks = assigned_tasks,
    task_summary = task_summary
  ))
}
