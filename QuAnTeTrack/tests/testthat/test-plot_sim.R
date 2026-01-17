test_that("plot_sim returns a ggplot object for default simulation with PaluxyRiver dataset", {
  simulated_tracks <- suppressWarnings(simulate_track(PaluxyRiver, nsim = 10))
  plot_result <- plot_sim(PaluxyRiver, simulated_tracks)

  expect_s3_class(plot_result, "ggplot")
})

test_that("plot_sim returns a ggplot object for Directed model simulation with PaluxyRiver dataset", {
  simulated_tracks_directed <- simulate_track(PaluxyRiver, nsim = 10, model = "Directed")
  plot_result <- plot_sim(PaluxyRiver, simulated_tracks_directed,
    colours_sim = c("#E69F00", "#56B4E9"), alpha_sim = 0.4, lwd_sim = 1,
    colours_act = c("black", "black"), alpha_act = 0.7, lwd_act = 2
  )

  expect_s3_class(plot_result, "ggplot")
})

test_that("plot_sim returns a ggplot object for Constrained model simulation with PaluxyRiver dataset", {
  simulated_tracks_constrained <- simulate_track(PaluxyRiver, nsim = 10, model = "Constrained")
  plot_result <- plot_sim(PaluxyRiver, simulated_tracks_constrained,
    colours_sim = c("#E69F00", "#56B4E9"), alpha_sim = 0.6,
    lwd_sim = 0.1, alpha_act = 0.5, lwd_act = 2
  )

  expect_s3_class(plot_result, "ggplot")
})

test_that("plot_sim returns a ggplot object for Unconstrained model simulation with PaluxyRiver dataset", {
  simulated_tracks_unconstrained <- simulate_track(PaluxyRiver, nsim = 10, model = "Unconstrained")
  plot_result <- plot_sim(PaluxyRiver, simulated_tracks_unconstrained,
    colours_sim = c("#E69F00", "#56B4E9"), alpha_sim = 0.2,
    lwd_sim = 1, colours_act = c("#E69F00", "#56B4E9"), alpha_act = 0.9,
    lwd_act = 2
  )

  expect_s3_class(plot_result, "ggplot")
})

test_that("plot_sim returns a ggplot object for MountTom dataset", {
  sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
  simulated_tracks_mt <- suppressWarnings(simulate_track(sbMountTom, nsim = 10))
  plot_result <- plot_sim(sbMountTom, simulated_tracks_mt)

  expect_s3_class(plot_result, "ggplot")
})

test_that("plot_sim returns a ggplot object for Directed model simulation with MountTom dataset", {
  sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
  simulated_tracks_mt_directed <- simulate_track(sbMountTom, nsim = 10, model = "Directed")
  plot_result <- plot_sim(sbMountTom, simulated_tracks_mt_directed,
    colours_sim = c(
      "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
      "#CC79A7", "#999999", "#F4A300", "#6C6C6C", "#1F77B4"
    ),
    alpha_sim = 0.3, lwd_sim = 1.5, alpha_act = 0.8, lwd_act = 2
  )

  expect_s3_class(plot_result, "ggplot")
})

test_that("plot_sim returns a ggplot object for Constrained model simulation with MountTom dataset", {
  sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
  simulated_tracks_mt_constrained <- simulate_track(sbMountTom, nsim = 10, model = "Constrained")
  plot_result <- plot_sim(sbMountTom, simulated_tracks_mt_constrained,
    colours_sim = c(
      "#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#F781BF", "#A65628",
      "#FFFF33", "#8DD3C7", "#FB8072", "#80BF91", "#F7F7F7"
    ),
    alpha_sim = 0.5, lwd_sim = 0.2, alpha_act = 0.6, lwd_act = 2
  )

  expect_s3_class(plot_result, "ggplot")
})

test_that("plot_sim returns a ggplot object for Unconstrained model simulation with MountTom dataset", {
  sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
  simulated_tracks_mt_unconstrained <- simulate_track(sbMountTom, nsim = 10, model = "Unconstrained")
  plot_result <- plot_sim(sbMountTom, simulated_tracks_mt_unconstrained,
    colours_sim = c(
      "#6BAED6", "#FF7F00", "#1F77B4", "#D62728", "#2CA02C", "#9467BD",
      "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"
    ),
    alpha_sim = 0.2, lwd_sim = 0.5, colours_act = c(
      "#6BAED6", "#FF7F00", "#1F77B4",
      "#D62728", "#2CA02C", "#9467BD", "#8C564B", "#E377C2",
      "#7F7F7F", "#BCBD22", "#17BECF"
    ), alpha_act = 1, lwd_act = 2
  )

  expect_s3_class(plot_result, "ggplot")
})

test_that("plot_sim handles invalid inputs correctly", {
  expect_error(plot_sim(NULL, NULL), "The 'data' argument must be a 'track' R object, which is a list consisting of two elements.")
  expect_error(plot_sim(PaluxyRiver, NULL), "The 'sim' argument must be a 'track simulation' R object, where each object is a list of simulated trajectories.")
  expect_error(
    plot_sim(PaluxyRiver, suppressWarnings(simulate_track(PaluxyRiver)), colours_sim = c("#E69F00")),
    "The length of 'colours_sim' does not match the number of unique simulated trajectories. Please ensure 'colours_sim' has as many colors as there are unique simulated trajectories."
  )
  expect_error(
    plot_sim(PaluxyRiver, suppressWarnings(simulate_track(PaluxyRiver)), colours_act = c("#E69F00")),
    "The length of 'colours_act' does not match the number of unique actual trajectories. Please ensure 'colours_act' has as many colors as there are unique actual trajectories."
  )
  expect_error(
    plot_sim(PaluxyRiver, suppressWarnings(simulate_track(PaluxyRiver)), alpha_sim = -0.5),
    "Alpha value for simulated trajectories 'alpha_sim' should be between 0 and 1."
  )
  expect_error(
    plot_sim(PaluxyRiver, suppressWarnings(simulate_track(PaluxyRiver)), alpha_act = 1.5),
    "Alpha value for actual trajectories 'alpha_act' should be between 0 and 1."
  )
  expect_error(
    plot_sim(PaluxyRiver, suppressWarnings(simulate_track(PaluxyRiver)), lwd_sim = 0),
    "Line width value for simulated trajectories 'lwd_sim' should be a positive number."
  )
  expect_error(
    plot_sim(PaluxyRiver, suppressWarnings(simulate_track(PaluxyRiver)), lwd_act = -1),
    "Line width value for actual trajectories 'lwd_act' should be a positive number."
  )
})
