province_names <-  plotDK::province_info$province_names
province_numbers <- plotDK::province_info$province_numbers
region_names <- plotDK::region_info$region_names
region_numbers <- plotDK::region_info$region_numbers
municipality_names <- plotDK::municipality_info$municipality_names
municipality_numbers <- plotDK::municipality_info$municipality_numbers
zipcode_numbers <- plotDK::zipcode_info$zipcode_numbers


context("Test create_plot_data() with NULL data input")

test_that(
  "create_plot_data() returns unchanged geomdata on the municipality plotlevel, if data input is NULL.", {
    plotdata <- plotDK:::create_plot_data(
      data = NULL,
      id = NULL,
      plotlevel = "municipality"
    )
    expect_is(plotdata, "data.frame")
    expect_identical(
      plotdata,
      plotDK::municipality
    )
    
  }
)

test_that(
  "create_plot_data() returns unchanged geomdata on the province plotlevel, if data input is NULL.", {
    plotdata <- plotDK:::create_plot_data(
      data = NULL,
      id = NULL,
      plotlevel = "province"
    )
    expect_is(plotdata, "data.frame")
    expect_identical(
      plotdata,
      plotDK::province
    )
    
  }
)

test_that(
  "create_plot_data() returns unchanged geomdata on the zipcode plotlevel, if data input is NULL.", {
    plotdata <- plotDK:::create_plot_data(
      data = NULL,
      id = NULL,
      plotlevel = "zipcode"
    )
    expect_is(plotdata, "data.frame")
    expect_identical(
      plotdata,
      plotDK::zipcodes
    )
    
  }
)

context("Test that create_plot_data() merges inputdata correctly when all known entities are provided")

test_that(
  "create_plot_data() merges correctly, when all known provinces are provided as character", {
    
    provincedata_character_id <- data.frame(
      province_name = province_names,
      value = 1:11,
      stringsAsFactors = FALSE
    )
    
    plotdata <- plotDK:::create_plot_data(
      data = provincedata_character_id, 
      id = "province_name",
      value = "value",
      plotlevel = "province"
    )
    
    expect_is(plotdata, "data.frame")
    expect_setequal(
      plotdata$id,
      plotDK::province$id
    )
    expect_setequal(
      plotdata$value,
      provincedata_character_id$value
    )
    
  }
)

test_that(
  "create_plot_data() merges correctly, when all known provinces are provided as numerics", {
    
    provincedata_numeric_id <- data.frame(
      province_number = province_numbers,
      value = 1:11,
      stringsAsFactors = FALSE
    )
    
    plotdata <- plotDK:::create_plot_data(
      data = provincedata_numeric_id, 
      id = "province_number",
      value = "value",
      plotlevel = "province"
    )
    
    expect_is(plotdata, "data.frame")
    expect_setequal(
      plotdata$id,
      plotDK::province$id
    )
    expect_setequal(
      plotdata$value,
      provincedata_numeric_id$value
    )
    
  }
)

test_that(
  "create_plot_data() merges correctly, when all known regions are provided as character", {
    
    regiondata_character_id <- data.frame(
      region_name = region_names,
      value = 1:5,
      stringsAsFactors = FALSE
    )
    
    plotdata <- plotDK:::create_plot_data(
      data = regiondata_character_id, 
      id = "region_name",
      value = "value",
      plotlevel = "region"
    )
    
    expect_is(plotdata, "data.frame")
    expect_setequal(
      plotdata$id,
      plotDK::region$id
    )
    expect_setequal(
      plotdata$value,
      regiondata_character_id$value
    )
    
  }
)

test_that(
  "create_plot_data() merges correctly, when all known regions are provided as numerics", {
    
    regiondata_numeric_id <- data.frame(
      region_number = region_numbers,
      value = 1:5,
      stringsAsFactors = FALSE
    )
    
    plotdata <- plotDK:::create_plot_data(
      data = regiondata_numeric_id, 
      id = "region_number",
      value = "value",
      plotlevel = "region"
    )
    
    expect_is(plotdata, "data.frame")
    expect_setequal(
      plotdata$id,
      plotDK::region$id
    )
    expect_setequal(
      plotdata$value,
      regiondata_numeric_id$value
    )
    
  }
)


test_that(
  "create_plot_data() merges correctly, when all known municipalities are provided as characters", {
    
    municipality_data_character_id <- data.frame(
      municipality_name = municipality_names,
      value = seq_len(length(municipality_names)),
      stringsAsFactors = FALSE
    )
    
    plotdata <- plotDK:::create_plot_data(
      data = municipality_data_character_id, 
      id = "municipality_name",
      value = "value",
      plotlevel = "municipality"
    )
    
    expect_is(plotdata, "data.frame")
    expect_setequal(
      plotdata$id,
      plotDK::municipality$id
    )
    expect_setequal(
      plotdata$value,
      municipality_data_character_id$value
    )
    
  }
)

test_that(
  "create_plot_data() merges correctly, when all known municipalities are provided as numerics", {
    
    municipality_data_numeric_id <- data.frame(
      municipality_number = municipality_numbers,
      value = seq_len(length(municipality_numbers)),
      stringsAsFactors = FALSE
    )
    
    plotdata <- plotDK:::create_plot_data(
      data = municipality_data_numeric_id, 
      id = "municipality_number",
      value = "value",
      plotlevel = "municipality"
    )
    
    expect_is(plotdata, "data.frame")
    expect_setequal(
      plotdata$id,
      plotDK::municipality$id
    )
    expect_setequal(
      plotdata$value,
      municipality_data_numeric_id$value
    )
    
  }
)

test_that(
  "create_plot_data() merges correctly, when all known zipcodes are provided as numbers.", {
    
    zipcode_data_numeric_id <- data.frame(
      zipcode_number = zipcode_numbers,
      value = seq_len(length(zipcode_numbers)),
      stringsAsFactors = FALSE
    )
    
    plotdata <- plotDK:::create_plot_data(
      data = zipcode_data_numeric_id, 
      id = "zipcode_number",
      value = "value",
      plotlevel = "zipcode"
    )
    
    expect_is(plotdata, "data.frame")
    expect_setequal(
      plotdata$id,
      plotDK::zipcodes$id
    )
    expect_setequal(
      plotdata$value,
      zipcode_data_numeric_id$value
    )
    
  }
)

context("Test that create_plot_data() merges inputdata correctly when only some known entities are provided")

test_that(
  "create_plot_data() merges correctly, when only a subset of municipalities are provided, and show_missing = FALSE", {
    
    municipality_subset <- data.frame(
      municipality_name = municipality_names[1:10],
      value = 1:10,
      stringsAsFactors = FALSE
    )
    
    plotdata <- plotDK:::create_plot_data(
      data = municipality_subset, 
      id = "municipality_name",
      value = "value",
      plotlevel = "municipality"
    )
    
    expect_is(plotdata, "data.frame")
    expect_setequal(
      plotdata$id,
      municipality_subset$municipality_name
    )
    expect_setequal(
      plotdata$value,
      municipality_subset$value
    )
    
  }
)


test_that(
  "create_plot_data() merges correctly, when only a subset of municipalities are provided, and show_missing = TRUE", {
    municipality_subset <- data.frame(
      municipality_name = municipality_names[1:10],
      value = 1:10,
      stringsAsFactors = FALSE
    )
    
    plotdata <- plotDK:::create_plot_data(
      data = municipality_subset, 
      id = "municipality_name",
      value = "value",
      plotlevel = "municipality",
      show_missing = TRUE
    )
    
    expect_is(plotdata, "data.frame")
    expect_true(
      !setequal(
        plotdata$id,
        municipality_subset$municipality_name
      )
    )
    
    expect_setequal(
      plotdata$id,
      plotDK::municipality$id
    )
    
    contained <- plotdata[plotdata$id %in% municipality_subset$municipality_name,]
    
    not_contained <-  plotdata[!plotdata$id %in% municipality_subset$municipality_name,]
    
    expect_setequal(
      contained$id,
      municipality_subset$municipality_name
    )
    
    expect_setequal(
      not_contained$id,
      setdiff(
        plotDK::municipality$id,
        contained$id
      )
    )
    
    expect_true(
      sum(is.na(not_contained$value)) == nrow(not_contained)
    )
    
  }
)


