# tiger_places_sf()

    Code
      kentucky_fips <- usmap::fips(state = "kentucky")
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      major_places_express <- expression(NAME %in% c("Bardstown", "Bowling Green",
        "Louisville", "Versailles", "Owensboro", "Frankfort", "Elizabethtown",
        "Danville"))
      kentucky_places_sf <- RcensusPkg::tiger_places_sf(state = kentucky_fips,
        express = major_places_express, general = TRUE, output_dir = output_dir,
        do_progress = FALSE, delete_files = TRUE)
      a_plot <- RplotterPkg::create_sf_plot(kentucky_places_sf)

