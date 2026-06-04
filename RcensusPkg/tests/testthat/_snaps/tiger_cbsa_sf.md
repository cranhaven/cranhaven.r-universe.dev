# tiger_cbsa_sf()

    Code
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      cbsa_tx_sf <- RcensusPkg::tiger_cbsa_sf(vintage = 2020, resol = "20m",
        state_filter = "TX", general = TRUE, output_dir = output_dir, do_progress = FALSE,
        delete_files = TRUE)
      a_plot <- RplotterPkg::create_sf_plot(cbsa_tx_sf)

