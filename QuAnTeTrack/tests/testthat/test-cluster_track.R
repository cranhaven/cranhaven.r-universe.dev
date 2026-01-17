test_that("cluster_track works correctly with MountTom and PaluxyRiver", {
  # Tests with MountTom dataset
  H_mounttom <- c(
    1.380, 1.404, 1.320, 1.736, 1.364, 1.432, 1.508, 1.768, 1.600,
    1.848, 1.532, 1.532, 0.760, 1.532, 1.688, 1.620, 0.636, 1.784,
    1.676, 1.872, 1.648, 1.760, 1.612
  )
  veltrack_MountTom <- velocity_track(MountTom, H = H_mounttom)

  expect_warning(cluster_track(MountTom, veltrack_MountTom, variables = c("TurnAng", "Velocity")), "12 tracks were discarded for having fewer than 4 footprints. Discarded track indices: 5, 6, 10, 11, 12, 14, 17, 19, 20, 21, 22, 23")

  result1 <- suppressWarnings(cluster_track(MountTom, veltrack_MountTom, variables = c("TurnAng", "Velocity")))
  expect_type(result1$clust$classification, "double")

  result2 <- suppressWarnings(cluster_track(MountTom, veltrack_MountTom, variables = c("Sinuosity", "StLength")))
  expect_s3_class(result2$clust, "Mclust")

  result3 <- suppressWarnings(cluster_track(MountTom, veltrack_MountTom, variables = c("MaxVelocity", "MinVelocity")))
  expect_type(result3$clust$classification, "double")

  result4 <- suppressWarnings(cluster_track(MountTom, veltrack_MountTom, variables = "Straightness"))
  expect_type(result4$clust$classification, "double")

  # Tests with PaluxyRiver dataset
  H_paluxyriver <- c(3.472, 2.200)
  Method_paluxyriver <- c("A", "B")
  veltrack_PaluxyRiver <- velocity_track(PaluxyRiver, H = H_paluxyriver, method = Method_paluxyriver)

  result5 <- cluster_track(PaluxyRiver, veltrack_PaluxyRiver, variables = c("Distance", "Straightness"))
  expect_true(is.data.frame(result5$matrix))
  expect_type(result5$clust$classification, "double")

  result6 <- cluster_track(PaluxyRiver, veltrack_PaluxyRiver, variables = c("Length", "sdVelocity"))
  expect_s3_class(result6$clust, "Mclust")

  result7 <- cluster_track(PaluxyRiver, veltrack_PaluxyRiver, variables = c("TurnAng", "sdTurnAng"))
  expect_type(result7$clust$classification, "double")

  result8 <- cluster_track(PaluxyRiver, veltrack_PaluxyRiver, variables = c("Sinuosity"))
  expect_type(result8$clust$classification, "double")
})
