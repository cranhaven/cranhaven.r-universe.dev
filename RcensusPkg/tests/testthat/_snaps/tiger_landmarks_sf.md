# tiger_landmarks_sf()

    Code
      kentucky_fips <- usmap::fips(state = "kentucky")
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      kentucky_landmarks_sf <- RcensusPkg::tiger_landmarks_sf(state = kentucky_fips,
        entity = "point", check_na = TRUE, output_dir = output_dir, do_progress = FALSE,
        delete_files = TRUE)
      a_plot <- RplotterPkg::create_sf_plot(kentucky_landmarks_sf)

