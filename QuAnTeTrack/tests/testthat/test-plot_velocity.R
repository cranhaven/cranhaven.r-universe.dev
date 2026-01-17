test_that("plot_velocity returns a ggplot object for velocity parameter with MountTom dataset", {
  H_mounttom <- c(
    1.380, 1.404, 1.320, 1.736, 1.364, 1.432, 1.508, 1.768, 1.600, 1.848,
    1.532, 1.532, 0.760, 1.532, 1.688, 1.620, 0.636, 1.784, 1.676, 1.872,
    1.648, 1.760, 1.612
  )

  V_mounttom <- velocity_track(MountTom, H = H_mounttom)
  plot_result <- plot_velocity(MountTom, V_mounttom, param = "V")

  expect_s3_class(plot_result, "ggplot")
})

test_that("plot_velocity returns a ggplot object for relative stride length parameter with PaluxyRiver dataset", {
  H_paluxyriver <- c(3.472, 2.200)
  Method_paluxyriver <- c("A", "B")

  V_paluxyriver <- velocity_track(PaluxyRiver, H = H_paluxyriver, method = Method_paluxyriver)
  plot_result <- plot_velocity(PaluxyRiver, V_paluxyriver, param = "RSL")

  expect_s3_class(plot_result, "ggplot")
})

test_that("plot_velocity returns a ggplot object for velocity with custom line width and colors (MountTom dataset)", {
  H_mounttom <- c(
    1.380, 1.404, 1.320, 1.736, 1.364, 1.432, 1.508, 1.768, 1.600, 1.848,
    1.532, 1.532, 0.760, 1.532, 1.688, 1.620, 0.636, 1.784, 1.676, 1.872,
    1.648, 1.760, 1.612
  )

  V_mounttom <- velocity_track(MountTom, H = H_mounttom)

  custom_colours <- c("blue", "green", "yellow", "red")
  custom_lwd <- 2

  plot_result <- plot_velocity(MountTom, V_mounttom, param = "V", lwd = custom_lwd, colours = custom_colours)

  expect_s3_class(plot_result, "ggplot")
})

test_that("plot_velocity returns a ggplot object for relative stride length with custom colors and no legend (PaluxyRiver dataset)", {
  H_paluxyriver <- c(3.472, 2.200)
  Method_paluxyriver <- c("A", "B")

  V_paluxyriver <- velocity_track(PaluxyRiver, H = H_paluxyriver, method = Method_paluxyriver)

  custom_colours_rsl <- c("purple", "orange", "pink", "gray")
  custom_lwd_rsl <- 1.5

  plot_result <- plot_velocity(PaluxyRiver, V_paluxyriver, param = "RSL", lwd = custom_lwd_rsl, colours = custom_colours_rsl, legend = FALSE)

  expect_s3_class(plot_result, "ggplot")
})

test_that("plot_velocity handles invalid inputs correctly", {
  H_mounttom <- c(
    1.380, 1.404, 1.320, 1.736, 1.364, 1.432, 1.508, 1.768, 1.600, 1.848,
    1.532, 1.532, 0.760, 1.532, 1.688, 1.620, 0.636, 1.784, 1.676, 1.872,
    1.648, 1.760, 1.612
  )
  expect_error(plot_velocity(NULL, NULL), "The 'data' argument must be a 'track' R object, which is a list consisting of two elements.")
  expect_error(plot_velocity(MountTom, NULL), "'trackvel' must be a list.")
  expect_error(plot_velocity(MountTom, velocity_track(MountTom, H = H_mounttom), param = "Invalid value for 'param'. Choose 'V' for velocity, 'RSL' for relative stride length, or NULL."))
  expect_error(plot_velocity(MountTom, velocity_track(MountTom, H = H_mounttom), colours = 5), "'colours' must be a character vector of color specifications.")
  expect_error(plot_velocity(MountTom, velocity_track(MountTom, H = H_mounttom), colours = c(2, 4)), "'colours' must be a character vector of color specifications.")
})
