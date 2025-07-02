test_that("wt_assign_tasks()", {
  expect_error(
    wt_assign_tasks("Not a proper file path"),
    "'wt_task_template_in' requires a data.frame or path to csv file"
  )

  set.seed(1231)
  r1_null_seed <- rnorm(100)

  expect_silent(
    task_output_file_no_seed <- wt_assign_tasks(
      wt_task_template_in = task_template,
      wt_task_output_file = NULL,
      interp_hours = template_observers,
      interp_hours_column = hrs,
      random_seed = NULL
    )
  )

  # Check that seed not changed by wt_assign_tasks() call
  r2_null_seed <- rnorm(100)
  expect(!any(r1_null_seed==r2_null_seed),
         "random_seed = NULL did not change global seed tick")

  set.seed(1231)
  r1 <- rnorm(100)

  set.seed(1231)
  expect_silent(
    task_output <- wt_assign_tasks(
      wt_task_template_in = task_template,
      wt_task_output_file = NULL,
      interp_hours = template_observers,
      interp_hours_column = hrs,
      random_seed = 65122
    )
  )

  # Check that seed not changed by wt_assign_tasks() call
  r2 <- rnorm(100)
  expect_equal(r1, r2)


  # Try from file
  tf <- tempfile(fileext = ".csv")
  tf_tasks <- tempfile(fileext = ".csv")
  readr::write_csv(template_observers, tf)
  readr::write_csv(task_template, tf_tasks)
  expect_no_warning(
    task_output_file <- wt_assign_tasks(
      wt_task_template_in = tf_tasks,
      wt_task_output_file = NULL,
      interp_hours = tf,
      interp_hours_column = hrs,
      random_seed = 65122
    )
  )



  # Compare file and data_table version
  expect_equal(task_output$assigned_tasks, task_output_file$assigned_tasks)

  tf_no_csv <- tempfile(fileext = ".xlsx")
  readr::write_csv(mtcars, tf_no_csv)
  expect_error(task_output_file <- wt_assign_tasks(
    wt_task_template_in = tf_no_csv,
    wt_task_output_file = NULL,
    interp_hours = template_observers,
    interp_hours_column = hrs,
    random_seed = 65122
  ), "'wt_task_template_in' must be a path")

  expect_error(task_output_file <- wt_assign_tasks(
    wt_task_template_in = task_template,
    wt_task_output_file = NULL,
    interp_hours = tf_no_csv,
    interp_hours_column = hrs,
    random_seed = 65122
  ), "'interp_hours' must be a path ")

  expect_error(task_output_file <- wt_assign_tasks(
    wt_task_template_in = task_template,
    wt_task_output_file = NULL,
    interp_hours = "Bad file path",
    interp_hours_column = hrs,
    random_seed = 65122
  ), "'interp_hours' requires a data.frame")

  # Check write
  tf_out <- tempfile(fileext = ".csv")
  task_output_written <- wt_assign_tasks(
    wt_task_template_in = task_template,
    wt_task_output_file = tf_out,
    interp_hours = template_observers,
    interp_hours_column = hrs,
    random_seed = 65122
  )
  tf_out_in <- readr::read_csv(tf_out,
    col_types = glue::glue_collapse(rep("c", 13)),
    na = character()
  )

  expect_equal(task_output_written$assigned_tasks, tf_out_in)
})
