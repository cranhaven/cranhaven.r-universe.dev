# tiger_county_subsection_sf()

    Code
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      ohio_hc_fips <- usmap::fips(state = "ohio", county = "holmes")
      ohio_fips <- substr(ohio_hc_fips, 1, 2)
      hc_fips <- substr(ohio_hc_fips, 3, 5)
      express <- parse(text = paste0("COUNTYFP == ", "\"", hc_fips, "\""))
      hc_ctysub_sf <- RcensusPkg::tiger_county_subsection_sf(state = ohio_fips,
        vintage = 2020, general = TRUE, express = express, output_dir = output_dir,
        do_progress = FALSE, delete_files = TRUE)
      a_plot <- RplotterPkg::create_sf_plot(hc_ctysub_sf)

