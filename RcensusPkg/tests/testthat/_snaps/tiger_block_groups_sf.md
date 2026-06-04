# tiger_block_groups_sf()

    Code
      dc_fips <- usmap::fips(state = "dc")
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      dc_block_groups_sf <- RcensusPkg::tiger_block_groups_sf(state = dc_fips,
        output_dir = output_dir, do_progress = FALSE, delete_files = TRUE)

