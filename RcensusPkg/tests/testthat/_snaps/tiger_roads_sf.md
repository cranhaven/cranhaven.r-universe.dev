# tiger_roads_sf()

    Code
      oh_fips <- usmap::fips(state = "ohio")
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      ohio_roads_sf <- RcensusPkg::tiger_roads_sf(state = oh_fips, entity = "state_roads",
        output_dir = output_dir, do_progress = FALSE, delete_files = TRUE)
      a_plot <- RplotterPkg::create_sf_plot(ohio_roads_sf)

