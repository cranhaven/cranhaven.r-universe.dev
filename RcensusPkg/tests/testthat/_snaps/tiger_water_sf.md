# tiger_water_sf() area

    Code
      state_county_fips <- usmap::fips(state = "Ohio", county = "Geauga")
      state_fips <- substr(state_county_fips, 1, 2)
      county_fips <- substr(state_county_fips, 3, 5)
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      geauga_area_water_sf <- RcensusPkg::tiger_water_sf(state = state_fips, county = county_fips,
        output_dir = output_dir, do_progress = FALSE, delete_files = TRUE)
      a_plot <- RplotterPkg::create_sf_plot(geauga_area_water_sf)

