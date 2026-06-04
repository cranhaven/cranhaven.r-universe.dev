# tiger_states_sf()

    Code
      florida_fips <- usmap::fips(state = "florida")
      express <- parse(text = paste0("STATEFP == ", "\"", florida_fips, "\""))
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      florida_general_sf <- RcensusPkg::tiger_states_sf(general = TRUE, express = express,
        output_dir = output_dir, do_progress = FALSE, delete_files = TRUE)
      a_plot <- RplotterPkg::create_sf_plot(florida_general_sf)

