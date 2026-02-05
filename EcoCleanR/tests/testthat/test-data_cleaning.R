# ec_db_merge
test_that("ec_db_merge works", {
  db1 <- data.frame(
    species = "A",
    decimalLongitude = c(-120.2, -117.1, NA, NA),
    decimalLatitude = c(20.2, 34.1, NA, NA),
    catalogNumber = c("12345", "89888", "LACM8898", "SDNHM6767"),
    occurrenceStatus = c("present", NA, "ABSENT", "Present"),
    basisOfRecord = c("preserved_specimen", NA, "fossilspecimen", "material_sample"),
    source = "db1",
    abundance = c(1, 6, 23, 0)
  )

  db2 <- data.frame(
    species = "A",
    decimalLongitude = c(-120.2, -117.1, NA, NA),
    decimalLatitude = c(20.2, 34.1, NA, NA),
    catalogNumber = c("12345", "898828", "LACM82898", "SDNHM62767"),
    occurrenceStatus = c("present", "Absent", "present", "Present"),
    basisOfRecord = c("preserved_specimen", NA, "fossilspecimen", "material_sample"),
    source = "db2",
    abundance = c(1, 3, 8, 19)
  )
  result <- ec_db_merge(db_list = list(db1, db2), datatype = "modern")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(sum(table(result$catalogNumber) > 1), 1)
  result2 <- ec_db_merge(db_list = list(db1, db2), datatype = "fossil")
  expect_s3_class(result2, "data.frame")
  expect_equal(nrow(result2), 1)
})

# ec_rm_duplicate
test_that("ec_rm_duplicate works", {
  db1 <- data.frame(
    species = "A",
    decimalLongitude = c(-120.2, -117.1, NA, NA),
    decimalLatitude = c(20.2, 34.1, NA, NA),
    catalogNumber = c("12345", "89888", "LACM8898", "SDNHM6767"),
    occurrenceStatus = c("present", NA, "ABSENT", "Present"),
    basisOfRecord = c("preserved_specimen", NA, "fossilspecimen", "material_sample"),
    source = "db1",
    abundance = c(1, 6, 23, 1)
  )

  db2 <- data.frame(
    species = "A",
    decimalLongitude = c(-120.2, -117.1, NA, NA),
    decimalLatitude = c(20.2, 34.1, NA, NA),
    catalogNumber = c("12345", "898828", "LACM82898", "SDNHM62767"),
    occurrenceStatus = c("present", "Absent", "present", "Present"),
    basisOfRecord = c("preserved_specimen", NA, "fossilspecimen", "material_sample"),
    source = "db2",
    abundance = c(1, 3, 8, 19)
  )
  result <- ec_rm_duplicate(ec_db_merge(db_list = list(db1, db2), "modern"), catalogNumber = "catalogNumber", abundance = "abundance")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(sum(table(result$cleaned_catalog) > 1), 0)
})

# ec_worms_synonym (Testing)
test_that("ec_worms_synonym works", {
  test_data1 <- data.frame(
    scientificName = c("Mexacanthina lugubris", "Mexacanthina angelica", "Notareal species"),
    stringsAsFactors = FALSE
  )
  result <- ec_worms_synonym("Mexacanthina lugubris", test_data1)
  # Check it's a data frame
  expect_s3_class(result, "data.frame")
  # Check required columns exist
  expect_true(all(c("Accepted_syn_worms", "ecodata_syn_with_count") %in% colnames(result)))
  # Check the input species is in the WoRMS column
  expect_true("Mexacanthina lugubris" %in% result$Accepted_syn_worms)
  # Check record count formatting exists (e.g., "Mexacanthina lugubris (1)")
  expect_true(any(grepl("Mexacanthina lugubris \\([0-9]+\\)", result$ecodata_syn_with_count)))
})

# ec_flag_with_locality (Testing)
test_that("ec_flag_locality works", {
  test_data2 <- data.frame(
    decimalLongitude = c(NA, NA, NA, NA, NA, NA, NA),
    decimalLatitude = c(NA, NA, NA, NA, NA, NA, NA),
    coordinateUncertaintyInMeters = c(NA, NA, NA, NA, NA, NA, NA),
    cleaned_catalog = c("12345", "89888", "LACM8898", "SDNHM6767", "67676", "ABC", "M9099"),
    locality = c("Santa Cruz", NA, "Los Angeles", "N/A", "", "San Diego", NA),
    verbatimLocality = c(NA, "CA coast", "", "N/A", "Long Beach", NA, "")
  )
  expect_equal(sum(ec_flag_with_locality(test_data2)), 5)
}) # 4 records has potential to assign georeferences using GeoLocate tool

# ec_merge_corrected_coordinates (Testing)
test_that("ec_merge_corrected_coordinates works", {
  test_data2 <- data.frame(
    decimalLongitude = c(NA, NA, NA, NA, NA, NA, NA),
    decimalLatitude = c(NA, NA, NA, NA, NA, NA, NA),
    coordinateUncertaintyInMeters = c(NA, NA, NA, NA, NA, NA, NA),
    cleaned_catalog = c("12345", "89888", "LACM8898", "SDNHM6767", "67676", "ABC", "M9099"),
    locality = c("Santa Cruz", NA, "Los Angeles", "N/A", "", "San Diego", NA),
    verbatimLocality = c(NA, "CA coast", "", "N/A", "Long Beach", NA, "")
  )


  test_data_corrected <- data.frame(
    corrected_longitude = c(-120, -119.8, -118, NA, -117.5, -117.6, NA),
    corrected_latitude = c(20, 34, 33, NA, 30, 33.2, NA),
    corrected_uncertainty = c(9000, 80000, 50, NA, 4500, 80000, NA),
    cleaned_catalog = c("12345", "89888", "LACM8898", "SDNHM6767", "67676", "ABC", "M9099")
  )

  latitude <- "decimalLatitude"
  longitude <- "decimalLongitude"

  result <- ec_merge_corrected_coordinates(test_data_corrected, test_data2,
    catalog = "cleaned_catalog",
    latitude = "decimalLatitude",
    longitude = "decimalLongitude",
    uncertainty_col = "coordinateUncertaintyInMeters"
  )

  expect_equal(sum(!is.na(result[[latitude]])), 5)
}) # 5 records got correct georeferences

# ec_filter_by_uncertainty (Testing)
test_that("ec_filter_by_uncertainty works", {
  test_data22 <- data.frame(
    species = "A",
    decimalLongitude = c(-120, -117, -121, -119),
    decimalLatitude = c(20, 34, 32, 30),
    cleaned_catalog = c("12345", "89888", "LACM8898", "SDNHM6767"),
    locality = c(NA, NA, "Los Angeles, CA", "San Pedro, CA"),
    coordinateUncertaintyInMeters = c(1000, 2000, 9999900, 16000)
  )

  expect_equal(nrow(ec_filter_by_uncertainty(test_data22, uncertainty_col = "coordinateUncertaintyInMeters", percentile = 0.96, ask = FALSE)), 3)
}) # out of 4 records 1 record will be filtered.

# ec_flag_precision (Testing)
test_that("ec_flag_precision", {
  test_data3 <- data.frame(
    species = "Mexacanthina lugubris",
    decimalLongitude = c(-120.67, -78, -110, -60, -75.5, -130.78, -10.2, 5.4),
    decimalLatitude = c(20.7, 34.6, 30.0, 10.5, 40.4, 25.66, 15.0, 35.9)
  )
  expect_equal(sum(ec_flag_precision(test_data3, longitude = "decimalLongitude", latitude = "decimalLatitude")), 2)
})

# ec_flag_non_region (testing)
test_that("ec_flag_non_region works", {
  test_data4 <- data.frame(
    species = "A",
    decimalLongitude = c(-120, -78, -110, -60, -75, -130, -10, 5),
    decimalLatitude = c(20, 34, 30, 10, 40, 25, 15, 35)
  )
  direction <- "east"
  ocean <- "pacific"
  buffer <- 25000

  # skip("Skipping ec_flag_non_region test - big shape files required to load for it")
  # skip_on_cran() # skip on cran due to heaving data load
  # skip_on_ci()
  expect_equal(sum(ec_flag_non_region(direction, ocean, buffer, test_data4)), 6)
})

# ec_extract_env_layers(testing)
test_that("ec_extract_env_layers works", {
  test_data5 <- data.frame(
    scientificName = "Mexacanthina lugubris",
    decimalLongitude = -116.24,
    decimalLatitude = 30.8
  )
  env_layers <- "BO_sstmean"
  # skip("Skipping ec_extract_env_layers test - big shape files required to load for it")
  # skip_on_cran() # skip on cran as external env layers load
  # skip_on_ci()
  result <- ec_extract_env_layers(test_data5, env_layers, latitude = "decimalLatitude", longitude = "decimalLongitude")
  expect_s3_class(result, "data.frame")
  expect_true(all(env_layers %in% colnames(result)))
  expect_equal(nrow(result), nrow(test_data5))
})

# ec_impute_env_values (Testing)
test_that("ec_impute_env_values works", {
  test_data6 <- data.frame(
    decimalLongitude = c(-117.6, -117.6, -117.55, -116.9),
    decimalLatitude = c(32.9, 32.8, 32.77, 31.9),
    temperature_mean = c(12, NA, 20.5, 14),
    temperature_min = c(9, NA, 15.2, 10),
    temperature_max = c(14, NA, 11.5, 18)
  )

  radius_km <- 30
  iter <- 3
  result <- ec_impute_env_values(
    test_data6,
    latitude = "decimalLatitude",
    longitude = "decimalLongitude",
    radius_km,
    iter
  )
  expect_s3_class(result, "data.frame")
  expect_true(!any(is.na(result))) # for this dataset
})

# ec_flag_outlier (Testing)
test_that("ec_flag_outlier works", {
  test_data7 <- data.frame(
    decimalLatitude = c(
      8.40, 16.80, 19.28, 20.25, 20.25, 20.25, 22.92, 23.22, 23.95, 24.36,
      24.40, 24.40, 24.40, 24.42, 24.52, 24.53, 24.54, 24.55, 24.55, 24.60, 24.61, 24.63, 24.63
    ),
    decimalLongitude = c(
      -83.28, -99.90, -104.87, -105.58, -105.58, -105.58, -106.10, -106.42, -110.88, -110.34,
      -112.07, -112.07, -112.07, -111.73, -111.95, -112.03, -112.07, -112.07, -112.07,
      -112.08, -112.10, -112.02, -112.12
    ),
    BO_sstmean = c(
      29.31, 29.36, 27.83, 28.02, 28.02, 28.02, 29.04, 26.73, 23.22, 25.11,
      21.88, 21.88, 21.88, 23.48, 22.40, 21.95, 21.95, 21.95, 21.95, 21.95, 21.49, 21.55, 21.49
    ),
    BO_sstmax = c(
      30.17, 31.31, 30.71, 32.42, 32.42, 32.42, 32.68, 31.30, 27.82, 30.01,
      27.23, 27.23, 27.23, 28.61, 27.44, 27.19, 27.19, 27.19, 27.19, 27.19, 26.94, 26.74, 26.94
    ),
    BO_sstmin = c(
      28.49, 27.18, 24.69, 23.62, 23.62, 23.62, 24.96, 22.43, 18.87, 20.71,
      17.82, 17.82, 17.82, 20.49, 19.52, 18.11, 18.11, 18.11, 18.11, 18.11, 17.51, 17.85, 17.51
    )
  )

  env_layers <- c("BO_sstmean", "BO_sstmin", "BO_sstmax")

  result <- ec_flag_outlier(test_data7, latitude = "decimalLatitude", longitude = "decimalLongitude", env_layers, itr = 100, k = 1, geo_quantile = 0.99, maha_quantile = 0.99)
  expect_equal(sum(result$outliers) > 0, TRUE)
})

# ec_plot_distance (Testing)
test_that("ec_plot_distance works", {
  df1 <- data.frame(
    latitude = runif(5, 30, 35),
    longitude = runif(5, -120, -115),
    temperature = rnorm(5, 15, 2),
    pH = rnorm(5, 8, 0.1),
    geo_distance = runif(5, 0, 100),
    maha_distance = runif(5, 0, 10)
  )

  df2 <- data.frame(
    latitude = runif(5, 30, 35),
    longitude = runif(5, -120, -115),
    temperature = rnorm(5, 16, 2),
    pH = rnorm(5, 7.9, 0.1),
    geo_distance = runif(5, 0, 100),
    maha_distance = runif(5, 0, 10)
  )

  iteration_list <- list(df1, df2)
  expect_silent(ec_plot_distance(iteration_list, geo_quantile = 0.99, maha_quantile = 0.99, iterative = FALSE))
})

# ec_geographic_map_w_flag and flag_outlier
test_that("ec_geographic_map_w_flag works", {
  test_data7 <- data.frame(
    decimalLatitude = c(
      8.40, 16.80, 19.28, 20.25, 20.25, 20.25, 22.92, 23.22, 23.95, 24.36,
      24.40, 24.40, 24.40, 24.42, 24.52, 24.53, 24.54, 24.55, 24.55, 24.60, 24.61, 24.63, 24.63
    ),
    decimalLongitude = c(
      -83.28, -99.90, -104.87, -105.58, -105.58, -105.58, -106.10, -106.42, -110.88, -110.34,
      -112.07, -112.07, -112.07, -111.73, -111.95, -112.03, -112.07, -112.07, -112.07,
      -112.08, -112.10, -112.02, -112.12
    ),
    BO_sstmean = c(
      29.31, 29.36, 27.83, 28.02, 28.02, 28.02, 29.04, 26.73, 23.22, 25.11,
      21.88, 21.88, 21.88, 23.48, 22.40, 21.95, 21.95, 21.95, 21.95, 21.95, 21.49, 21.55, 21.49
    ),
    BO_sstmax = c(
      30.17, 31.31, 30.71, 32.42, 32.42, 32.42, 32.68, 31.30, 27.82, 30.01,
      27.23, 27.23, 27.23, 28.61, 27.44, 27.19, 27.19, 27.19, 27.19, 27.19, 26.94, 26.74, 26.94
    ),
    BO_sstmin = c(
      28.49, 27.18, 24.69, 23.62, 23.62, 23.62, 24.96, 22.43, 18.87, 20.71,
      17.82, 17.82, 17.82, 20.49, 19.52, 18.11, 18.11, 18.11, 18.11, 18.11, 17.51, 17.85, 17.51
    )
  )

  env_layers <- c("BO_sstmean", "BO_sstmin", "BO_sstmax")
  test_data7$outliers <- (ec_flag_outlier(test_data7, latitude = "decimalLatitude", longitude = "decimalLongitude", env_layers, itr = 100, k = 1, geo_quantile = 0.99, maha_quantile = 0.99))$outlier # result obtained from ec_flag_outliers

  p <- ec_geographic_map_w_flag(test_data7,
    flag_column = "outliers",
    latitude = "decimalLatitude",
    longitude = "decimalLongitude"
  )
  expect_s3_class(p, "ggplot")
})

# ec_geographic_map
test_that("ec_geographic_map works", {
  test_data7 <- data.frame(
    decimalLatitude = c(
      8.40, 16.80, 19.28, 20.25, 20.25, 20.25, 22.92, 23.22, 23.95, 24.36,
      24.40, 24.40, 24.40, 24.42, 24.52, 24.53, 24.54, 24.55, 24.55, 24.60, 24.61, 24.63, 24.63
    ),
    decimalLongitude = c(
      -83.28, -99.90, -104.87, -105.58, -105.58, -105.58, -106.10, -106.42, -110.88, -110.34,
      -112.07, -112.07, -112.07, -111.73, -111.95, -112.03, -112.07, -112.07, -112.07,
      -112.08, -112.10, -112.02, -112.12
    ),
    BO_sstmean = c(
      29.31, 29.36, 27.83, 28.02, 28.02, 28.02, 29.04, 26.73, 23.22, 25.11,
      21.88, 21.88, 21.88, 23.48, 22.40, 21.95, 21.95, 21.95, 21.95, 21.95, 21.49, 21.55, 21.49
    ),
    BO_sstmax = c(
      30.17, 31.31, 30.71, 32.42, 32.42, 32.42, 32.68, 31.30, 27.82, 30.01,
      27.23, 27.23, 27.23, 28.61, 27.44, 27.19, 27.19, 27.19, 27.19, 27.19, 26.94, 26.74, 26.94
    ),
    BO_sstmin = c(
      28.49, 27.18, 24.69, 23.62, 23.62, 23.62, 24.96, 22.43, 18.87, 20.71,
      17.82, 17.82, 17.82, 20.49, 19.52, 18.11, 18.11, 18.11, 18.11, 18.11, 17.51, 17.85, 17.51
    )
  )

  env_layers <- c("BO_sstmean", "BO_sstmin", "BO_sstmax")
  test_data7$outliers <- (ec_flag_outlier(test_data7, latitude = "decimalLatitude", longitude = "decimalLongitude", env_layers, itr = 100, k = 1, geo_quantile = 0.99, maha_quantile = 0.99))$outlier # result obtained from ec_flag_outliers
  p1 <- ec_geographic_map(test_data7,
    latitude = "decimalLatitude",
    longitude = "decimalLongitude"
  )
  expect_s3_class(p1, "ggplot")
})

# ec_var_summary
test_that("ec_var_summary works", {
  test_data7 <- data.frame(
    decimalLatitude = c(
      8.40, 16.80, 19.28, 20.25, 20.25, 20.25, 22.92, 23.22, 23.95, 24.36,
      24.40, 24.40, 24.40, 24.42, 24.52, 24.53, 24.54, 24.55, 24.55, 24.60, 24.61, 24.63, 24.63
    ),
    decimalLongitude = c(
      -83.28, -99.90, -104.87, -105.58, -105.58, -105.58, -106.10, -106.42, -110.88, -110.34,
      -112.07, -112.07, -112.07, -111.73, -111.95, -112.03, -112.07, -112.07, -112.07,
      -112.08, -112.10, -112.02, -112.12
    ),
    BO_sstmean = c(
      29.31, 29.36, 27.83, 28.02, 28.02, 28.02, 29.04, 26.73, 23.22, 25.11,
      21.88, 21.88, 21.88, 23.48, 22.40, 21.95, 21.95, 21.95, 21.95, 21.95, 21.49, 21.55, 21.49
    ),
    BO_sstmax = c(
      30.17, 31.31, 30.71, 32.42, 32.42, 32.42, 32.68, 31.30, 27.82, 30.01,
      27.23, 27.23, 27.23, 28.61, 27.44, 27.19, 27.19, 27.19, 27.19, 27.19, 26.94, 26.74, 26.94
    ),
    BO_sstmin = c(
      28.49, 27.18, 24.69, 23.62, 23.62, 23.62, 24.96, 22.43, 18.87, 20.71,
      17.82, 17.82, 17.82, 20.49, 19.52, 18.11, 18.11, 18.11, 18.11, 18.11, 17.51, 17.85, 17.51
    )
  )

  env_layers <- c("BO_sstmean", "BO_sstmin", "BO_sstmax")
  test_data7$outliers <- (ec_flag_outlier(test_data7, latitude = "decimalLatitude", longitude = "decimalLongitude", env_layers, itr = 100, k = 1, geo_quantile = 0.99, maha_quantile = 0.99))$outlier # result obtained from ec_flag_outliers
  test_data8 <- test_data7 %>%
    dplyr::filter(outliers == 0)


  result <- ec_var_summary(test_data8, latitude = "decimalLatitude", longitude = "decimalLongitude", env_layers)
  expect_s3_class(result, "data.frame")
  # Check required columns exist
  expect_true(all(c("Max", "Min", "Mean") %in% colnames(result)))
  # Check record count formatting exists (e.g., "Mexacanthina lugubris (1)")
  expect_true(!any(is.na(result)))
})

# ec_plot_var_range
test_that("ec_plot_var_range works", {
  test_data7 <- data.frame(
    decimalLatitude = c(
      8.40, 16.80, 19.28, 20.25, 20.25, 20.25, 22.92, 23.22, 23.95, 24.36,
      24.40, 24.40, 24.40, 24.42, 24.52, 24.53, 24.54, 24.55, 24.55, 24.60, 24.61, 24.63, 24.63
    ),
    decimalLongitude = c(
      -83.28, -99.90, -104.87, -105.58, -105.58, -105.58, -106.10, -106.42, -110.88, -110.34,
      -112.07, -112.07, -112.07, -111.73, -111.95, -112.03, -112.07, -112.07, -112.07,
      -112.08, -112.10, -112.02, -112.12
    ),
    BO_sstmean = c(
      29.31, 29.36, 27.83, 28.02, 28.02, 28.02, 29.04, 26.73, 23.22, 25.11,
      21.88, 21.88, 21.88, 23.48, 22.40, 21.95, 21.95, 21.95, 21.95, 21.95, 21.49, 21.55, 21.49
    ),
    BO_sstmax = c(
      30.17, 31.31, 30.71, 32.42, 32.42, 32.42, 32.68, 31.30, 27.82, 30.01,
      27.23, 27.23, 27.23, 28.61, 27.44, 27.19, 27.19, 27.19, 27.19, 27.19, 26.94, 26.74, 26.94
    ),
    BO_sstmin = c(
      28.49, 27.18, 24.69, 23.62, 23.62, 23.62, 24.96, 22.43, 18.87, 20.71,
      17.82, 17.82, 17.82, 20.49, 19.52, 18.11, 18.11, 18.11, 18.11, 18.11, 17.51, 17.85, 17.51
    )
  )

  env_layers <- c("BO_sstmean", "BO_sstmin", "BO_sstmax")
  test_data7$outliers <- (ec_flag_outlier(test_data7, latitude = "decimalLatitude", longitude = "decimalLongitude", env_layers, itr = 100, k = 1, geo_quantile = 0.99, maha_quantile = 0.99))$outlier # result obtained from ec_flag_outliers
  test_data8 <- test_data7 %>%
    dplyr::filter(outliers == 0)

  p2 <- ec_plot_var_range(test_data7, ec_var_summary(test_data8, latitude = "decimalLatitude", longitude = "decimalLongitude", env_layers), latitude = "decimalLatitude", longitude = "decimalLongitude", env_layers) # this is the final cleaned data table which will be used to derive summary of acceptable niche
  expect_s3_class(p2, "ggplot")
})

# ec_flag_non_east_atlantic (testing)
test_that("ec_flag_non_east_atlantic works", {
  test_data4 <- data.frame(
    species = "A",
    decimalLongitude = c(-120, -78, -110, -60, -75, -130, -10, 5),
    decimalLatitude = c(20, 34, 30, 10, 40, 25, 15, 35)
  )
  ocean_names <- c("North Atlantic Ocean", "South Atlantic Ocean")
  buffer_distance <- 25000
  skip("Skipping ec_flag_non_east_atlantic test - big shape files required to load for it")
  skip_on_cran() # skip on cran due to heaving data load

  expect_equal(sum(ec_flag_non_east_atlantic(
    ocean_names,
    buffer_distance,
    test_data4,
    latitude = "decimalLatitude",
    longitude = "decimalLongitude"
  )), 8)
})

# ec_flag_non_west_atlantic (testing)
test_that("ec_flag_non_west_atlantic works", {
  test_data4 <- data.frame(
    species = "A",
    decimalLongitude = c(-120, -78, -110, -60, -75, -130, -10, 5),
    decimalLatitude = c(20, 34, 30, 10, 40, 25, 15, 35)
  )
  ocean_names <- c("North Atlantic Ocean", "South Atlantic Ocean")
  buffer_distance <- 25000
  skip("Skipping ec_flag_non_west_atlantic test - big shape files required to load for it")
  skip_on_cran() # skip on cran due to heaving data load

  expect_equal(sum(ec_flag_non_west_atlantic(
    ocean_names,
    buffer_distance,
    test_data4,
    latitude = "decimalLatitude",
    longitude = "decimalLongitude"
  )), 5)
})

# ec_flag_non_west_pacific (testing)
test_that("ec_flag_non_west_pacific works", {
  test_data4 <- data.frame(
    species = "A",
    decimalLongitude = c(-120, -78, -110, -60, -75, -130, -10, 5),
    decimalLatitude = c(20, 34, 30, 10, 40, 25, 15, 35)
  )
  ocean_names <- c("North Pacific Ocean", "South Pacific Ocean")
  buffer_distance <- 25000
  skip("Skipping ec_flag_non_west_pacific test - big shape files required to load for it")
  skip_on_cran() # skip on cran due to heaving data load

  expect_equal(sum(ec_flag_non_west_pacific(
    ocean_names,
    buffer_distance,
    test_data4,
    latitude = "decimalLatitude",
    longitude = "decimalLongitude"
  )), 8)
})
