# tiger_zctas_sf()

    Code
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      express <- parse(text = "ZCTA5CE20 == \"02420\"")
      mun_zcta_sf <- RcensusPkg::tiger_zctas_sf(vintage = 2020, general = TRUE,
        express = express, output_dir = output_dir, do_progress = FALSE,
        delete_files = TRUE)
      a_plot <- RplotterPkg::create_sf_plot(mun_zcta_sf)

