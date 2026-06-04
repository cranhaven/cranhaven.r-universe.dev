# tiger_urban_area_sf()

    Code
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      us_urban_areas_sf <- RcensusPkg::tiger_urban_area_sf(vintage = 2019, general = TRUE,
        output_dir = output_dir, do_progress = FALSE, delete_files = TRUE)
      a_plot <- RplotterPkg::create_sf_plot(us_urban_areas_sf)

