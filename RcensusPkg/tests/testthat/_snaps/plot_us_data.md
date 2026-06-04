# plot_us_data() default

    Code
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      a_plot <- RcensusPkg::plot_us_data(title = "A Default Mapping of US States",
        output_dir = output_dir, delete_files = FALSE)

# plot_us_data() discrete

    Code
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      a_plot <- RcensusPkg::plot_us_data(df = RcensusPkg::vote2020, title = "US Presidential Vote 2020",
      states_col = "State", value_col = "Party", output_dir = output_dir,
      delete_files = FALSE, scale_breaks = c("R", "D"), scale_limits = c("R", "D"),
      scale_values = c("red", "blue"), scale_labels = c("Republican", "Democrat"),
      sf_color = "white")

