province_names <-  plotDK::province_info$province_names
province_numbers <- plotDK::province_info$province_numbers


context("Test check_data_input() in plotDK()")


provincedata <- data.frame(
  province_name = province_names,
  value = 1:11,
  stringsAsFactors = FALSE
)

test_that(
  "plotDK() returns error when the provided data is not a data.frame", {
    
    expect_error(
      plotDK::plotDK(
        data = c(1,2,3),
        id = "id",
        value = c(1,2,3),
        plotlevel = "province"
      ),
      regexp = "Inputdata must be a data.frame or similar"
    )

    
    expect_error(
      plotDK::plotDK(
        data = list(provincedata),
        id = "province_name",
        value = "value",
        plotlevel = "province"
      ),
      regexp = "Inputdata must be a data.frame or similar"
    )
    
  }
)

test_that(
  "plotDK() returns error when data is provided but the value argument is still NULL", {
    

    expect_error(
      plotDK::plotDK(
        data = provincedata,
        id = "province_name",
        plotlevel = "province"
      ),
      regexp = "When data is provided a value column must also be provided"
    )
    
  }
)

test_that(
  "plotDK() returns error when data is provided but the id argument is still NULL", {
    
    expect_error(
      plotDK::plotDK(
        data = provincedata,
        value = "value",
        plotlevel = "province"
      ),
      regexp = "When data is provided an id column must also be provided"
    )
    
  }
)

test_that(
  "plotDK() returns error when plotlevel is not in municipality, region, province, or zipcode", {
    expect_error(
      plotDK::plotDK(
        plotlevel = "somelevel"
      ),
      regexp = "Plotlevel must be either:"
    )
  }
)

test_that(
  "plotDK() returns error if all the provided id's are not valid", {
    provincedata <- data.frame(
      province_name = c("a", "b", "c"),
      province_numbers = c(20, 21, 22),
      value = c(1:3),
      stringsAsFactors = FALSE
    )
    
    expect_error(
      plotDK::plotDK(
        data = provincedata,
        id = "province_name",
        value = "value",
        plotlevel = "province"
      ),
      regexp = "No valid id's provided. "
    )
    
    expect_error(
      plotDK::plotDK(
        data = provincedata,
        id = "province_numbers",
        value = "value",
        plotlevel = "province"
      ),
      regexp = "No valid id's provided. "
    )

  }
)

test_that(
  "plotDK() returns warning if some of the provided id's are not valid", {
    provincedata <- data.frame(
      province_name = plotDK::province_info$province_names,
      province_numbers = plotDK::province_info$province_numbers,
      value = 1:11,
      stringsAsFactors = FALSE
    )
    
    provincedata[1, ]$province_name <- "a"
    provincedata[1, ]$province_numbers <- 20
    
    expect_warning(
      plotDK::plotDK(
        data = provincedata,
        id = "province_name",
        value = "value",
        plotlevel = "province"
      ),
      regexp = "The following provided id's are not recognized as valid and will not be plotted:"
    )
    
  }
)
