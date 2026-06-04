# tiger_tracts_sf()

    Code
      nm_los_alamos_fips <- usmap::fips(state = "new mexico", county = "los alamos")
      nm_fips <- substr(nm_los_alamos_fips, 1, 2)
      los_alamos_fips <- substr(nm_los_alamos_fips, 3, 5)
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      express <- parse(text = paste0("COUNTYFP == ", "\"", los_alamos_fips, "\""))
      losalamos_tracts_sf <- RcensusPkg::tiger_tracts_sf(state = nm_fips, general = TRUE,
        express = express, output_dir = output_dir, do_progress = FALSE,
        delete_files = TRUE)

