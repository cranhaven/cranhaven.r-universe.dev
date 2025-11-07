province_names <-  plotDK::province_info$province_names
province_numbers <- plotDK::province_info$province_numbers


context("Test output from plotDK()")

provincedata <- data.frame(
  province_name = province_names,
  value = 1:11,
  stringsAsFactors = FALSE
)

test_that(
  "plotDK() returns a ggplot object, when interactive = FALSE", {
    p <- plotDK::plotDK(
      data = provincedata,
      id = "province_name",
      value = "value",
      plotlevel = "province",
      interactive = FALSE
    )
    
    expect_is(p, "gg")
    
    p <- plotDK::plotDK()
    expect_is(p, "gg")
  }
)

test_that(
  "plotDK() returns a ggplot object, when interactive = TRUE", {
    p <- plotDK::plotDK(
      data = provincedata,
      id = "province_name",
      value = "value",
      plotlevel = "province",
      interactive = TRUE
    )
    expect_is(p, "plotly")
    
    p <- plotDK::plotDK(
      plotlevel = "province",
      interactive = TRUE
    )
    expect_is(p, "plotly")
    
  }
)

test_that(
  "The returned plot has the correct labels", {
    provincedata <- data.frame(
      province_name = province_names,
      test_label = 1:11,
      stringsAsFactors = FALSE
    )
    
    p <- plotDK::plotDK(
      data = provincedata,
      id = "province_name",
      value = "test_label",
      plotlevel = "province"
    )
    
    labels <- p$labels
    
    expect_setequal(
      c("x", "y", "group", "subgroup", "text", "fill"),
      names(labels)
    )
    
    expect_equal(
      labels$fill,
      "test_label"
    )
    
    p <- plotDK::plotDK()
    
    labels <- p$labels
    
    expect_equal(
      labels$fill,
      "fill"
    )
    
  }
)

