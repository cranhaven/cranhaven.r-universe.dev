test_that("plot_growth_performance() produces expected output", {
  
  # Generate test data
  test_stats_data <- tibble(data.frame(
    Well_Position = paste0(rep(LETTERS[1:8],
                               each = length(1:12)), "-",
                           rep(1:12,
                               times = length(LETTERS[1:8]))),
    Value = c(1:12, -13:-24, 25:96),
    Validity = c("valid", rep("invalid", 95)),
    Treatment = rep(LETTERS[1:8], each = 12),
    Concentration = rep(seq(from = 80, to = 10,
                            length.out=8), each = 12),
    Timepoint = "T0",
    File = "Some_file",
    Group = "Group A",
    Experiment = "Experiment 1"
  ))
  
  test_summarized_data <- tibble(data.frame(
    Group = "Group A",
    Treatment = LETTERS[1:8],
    Concentration = c(80,70,60,50,40,30,20,10),
    Timepoint = "T0",
    mean = c(6.5, -18.5, 30.5, 42.5, 54.5, 66.5, 78.5, 90.5),
    stderr = 1.04
  ))
  
  # Test case 1: Display data without applying sign test
  p1 <- plot_growth_performance(input_data = test_summarized_data)
  p1_layers <- sapply(p1$layers, function(x) class(x$geom)[1])
  expect_true("GeomBar" %in% p1_layers[1])
  expect_true("GeomErrorbar" %in% p1_layers[2])
  expect_true("GeomText" %in% p1_layers[3])
  expect_true("GeomHline" %in% p1_layers[4])
  expect_equal(unique(p1$data$p.signif), "") # no asterisk notation plotted
  
  # Test case 2: Display data with sign test applied and grouping structure
  p2 <- plot_growth_performance(input_data = test_summarized_data,
                                stats_data = test_stats_data,
                                apply_sign_test = T,
                                grouping = c("Treatment", "Concentration"))
  p2_layers <- sapply(p2$layers, function(x) class(x$geom)[1])
  expect_true("GeomBar" %in% p2_layers[1])
  expect_true("GeomErrorbar" %in% p2_layers[2])
  expect_true("GeomText" %in% p2_layers[3])
  expect_true("GeomHline" %in% p2_layers[4])
  expect_false(identical(p1,p2))
  expect_equal(unique(p2$data$p.signif), "***") # asterisk notation plotted
})

