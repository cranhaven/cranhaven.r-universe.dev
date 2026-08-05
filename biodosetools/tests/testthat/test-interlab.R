# ILC module ----

test_that("ILC module works", {
  fit_results_A1 <- app_sys("extdata", "A1_Estimation_results.rds") %>%
    readRDS()

  fit_results_A2 <- app_sys("extdata", "A2_Estimation_results.rds") %>%
    readRDS()

  list_lab_names <- list("A1", "A2")
  all_rds <- list(fit_results_A1, fit_results_A2)

  tables_list <- summary_curve_tables(
    num_labs = 2,
    list_lab_names,
    all_rds
  )

  expect_equal(colnames(tables_list[[1]]), c("Lab", "Module", "Type", "Sample", "N", "X", "estimate", "lower", "upper", "y", "y.err", "DI", "u"))
  expect_equal(colnames(tables_list[[2]]), c("Lab", "Module", "Type", "radiation quality", "calibration", "irradiation", "temperature", "dose rate",
                                             "curve origin", "C", "alpha", "beta", "C std.error", "alpha std.error", "beta std.error",  "max curve dose" ))


  expect_equal(tables_list[[1]][, "Lab"], rep(c("A1", "A2"), 3))
  expect_equal(tables_list[[1]][, "Module"], rep("dicentrics", 6))
  expect_equal(tables_list[[1]][, "Type"], rep("manual", 6))
  expect_equal(tables_list[[1]][, "Sample"], rep(c("1", "2", "3"), rep(2, 3)))
  expect_equal(tables_list[[1]][, "Module"], rep("dicentrics", 6))
  expect_equal(tables_list[[1]][, "N"], rep(200, 6))
  expect_equal(tables_list[[1]][, "X"], c(140, 111, 204, 147, 368, 253))
  expect_equal(round(tables_list[[1]][, "estimate"], 4), c(2.5892, 2.6110, 3.1461, 3.0187, 4.2594, 3.9892))
  expect_equal(round(tables_list[[1]][, "lower"], 4), c(2.0834, 2.2311, 2.6149, 2.6227, 3.8823, 3.5530))
  expect_equal(round(tables_list[[1]][, "upper"], 4), c(3.2089, 3.0456, 3.7859, 3.4713, 4.6889, 4.4873))
  expect_equal(round(tables_list[[1]][, "y"], 4), c(0.700, 0.555, 1.020, 0.735, 1.840, 1.265))
  expect_equal(round(tables_list[[1]][, "y.err"], 4), c(0.0610, 0.0495, 0.0733, 0.0556, 0.0923, 0.0785))
  expect_equal(round(tables_list[[1]][, "DI"], 4), c(1.0625, 0.8818, 1.0539, 0.8406, 0.9255, 0.9731))
  expect_equal(round(tables_list[[1]][, "u"], 4), c(0.6252, -1.1840,  0.5389, -1.5951, -0.7442, -0.2692))


  expect_equal(tables_list[[2]][, "Lab"], c("A1", "A2"))
  expect_equal(tables_list[[2]][, "Module"], rep("dicentrics", 2))
  expect_equal(tables_list[[2]][, "radiation quality"], c("Cs-137", "Co-60"))
  expect_equal(tables_list[[2]][, "calibration"], rep("air kerma", 2))
  expect_equal(tables_list[[2]][, "irradiation"], rep("air", 2))
  expect_equal(tables_list[[2]][, "temperature"], c("20", "37"))
  expect_equal(tables_list[[2]][, "dose rate"], c( "0.446", "0.27" ))
  expect_equal(tables_list[[2]][, "curve origin"], rep("own", 2))
  expect_equal(round(tables_list[[2]][, "C"], 5), c(0.00119, 0.00050))
  expect_equal(round(tables_list[[2]][, "alpha"], 5), c(0.01904, 0.01420))
  expect_equal(round(tables_list[[2]][, "beta"], 5), c(0.09688, 0.07590))
  expect_equal(round(tables_list[[2]][, "C std.error"], 6), c(0.000104, 0.000500))
  expect_equal(round(tables_list[[2]][, "alpha std.error"], 6), c(0.004119, 0.004400))
  expect_equal(round(tables_list[[2]][, "beta std.error"], 6), c(0.003505, 0.002700))
  expect_equal(tables_list[[2]][, "max curve dose"], c( 6.00, 5.05 ))

  # Test plots summary
  yield_boxplot(
    dat =  tables_list[[1]],
    place = "UI"
  )

  dose_boxplot(
    dat = tables_list[[1]],
    place = "UI"
  )

  u_test_plot(
    dat = tables_list[[1]],
    place = "UI"
  )

  DI_plot(
    dat = tables_list[[1]],
    place = "UI"
  )

  # Test plots curves
  c_plot <- curves_plot(
    dat = tables_list[[2]],
    curve = "manual",
    curve_type = "lin_quad",
    place = "UI"
  )

  expect_equal(names(c_plot$labels), c("x", "y", "title"))
  expect_equal(unname(unlist(c_plot$labels)), c("Dose (Gy)", "aberr/cell", "Manual curves plot"))

  bar_plots(
    dat = tables_list[[2]],
    curve = "manual",
    place = "UI"
  )

  # Test Z-score calculation for doses

  zscore_S1_algA <- calc.zValue.new(
    X =  tables_list[[1]][["estimate"]][1:2],
    type = "dose",
    alg = "algA",
    c = 2.56)

  zscore_S1_algB <- calc.zValue.new(
    X =  tables_list[[1]][["estimate"]][1:2],
    type = "dose",
    alg = "algB",
    c = 2.56)

  zscore_S1_QHampel <- calc.zValue.new(
    X =  tables_list[[1]][["estimate"]][1:2],
    type = "dose",
    alg = "QHampel",
    c = 2.56)

  expect_equal(round(zscore_S1_algA, 4), c(1.6777, 2.9254))
  expect_equal(round(zscore_S1_algB, 4), c(1.7724, 3.0907))
  expect_equal(round(zscore_S1_QHampel, 4), c(1.2118, 2.1130))


  zscore_S2_algA <- calc.zValue.new(
    X =  tables_list[[1]][["estimate"]][3:4],
    type = "dose",
    alg = "algA",
    c = 3.41)

  zscore_S2_algB <- calc.zValue.new(
    X =  tables_list[[1]][["estimate"]][3:4],
    type = "dose",
    alg = "algB",
    c = 3.41)

  zscore_S2_QHampel <- calc.zValue.new(
    X =  tables_list[[1]][["estimate"]][3:4],
    type = "dose",
    alg = "QHampel",
    c = 3.41)

  expect_equal(round(zscore_S2_algA, 4), c(-2.5857, -3.8334))
  expect_equal(round(zscore_S2_algB, 4), c(-2.7317, -4.0500))
  expect_equal(round(zscore_S2_QHampel, 4), c(-1.8676, -2.7689))

  zscore_S3_algA <- calc.zValue.new(
    X =  tables_list[[1]][["estimate"]][5:6],
    type = "dose",
    alg = "algA",
    c = 4.54)


  zscore_S3_algB <- calc.zValue.new(
    X =  tables_list[[1]][["estimate"]][5:6],
    type = "dose",
    alg = "algB",
    c = 4.54)

  zscore_S3_QHampel <- calc.zValue.new(
    X =  tables_list[[1]][["estimate"]][5:6],
    type = "dose",
    alg = "QHampel",
    c = 4.54)

  expect_equal(round(zscore_S3_algA, 4), c(-1.2959, -2.5437))
  expect_equal(round(zscore_S3_algB, 4), c(-1.3691, -2.6874))
  expect_equal(round(zscore_S3_QHampel, 4), c(-0.9360, -1.8373))

  data_frame_zscore <- data.frame(
    Lab = tables_list[[1]][["Lab"]],
    Sample = tables_list[[1]][["Sample"]],
    Type = tables_list[[1]][["Type"]],
    Reference = rep(c(2.56, 3.41, 4.54), 2),
    Dose = tables_list[[1]][["estimate"]],
    Deviation = tables_list[[1]][["estimate"]] - rep(c(2.56, 3.41, 4.54), 2),
    Zscore = c(zscore_S1_algA, zscore_S2_algA, zscore_S3_algA),
    stringsAsFactors = FALSE
  )

  expect_equal(colnames(data_frame_zscore), c("Lab", "Sample", "Type", "Reference", "Dose", "Deviation", "Zscore"))
  expect_equal(data_frame_zscore[, "Lab"], rep(c("A1", "A2"), 3))
  expect_equal(data_frame_zscore[, "Sample"], rep(c("1", "2", "3"), rep(2, 3)))
  expect_equal(data_frame_zscore[, "Reference"], rep(c(2.56, 3.41, 4.54), 2))
  expect_equal(round(data_frame_zscore[, "Dose"], 4), c(2.5892, 2.6110, 3.1461, 3.0187, 4.2594, 3.9892))
  expect_equal(round(data_frame_zscore[, "Deviation"], 4), c(0.0292, -0.7990, -1.3939,  0.4587,  0.8494, -0.5508))
  expect_equal(round(data_frame_zscore[, "Zscore"], 4), c(1.6777,  2.9254, -2.5857, -3.8334, -1.2959, -2.5437))

 plot_1 <- plot_zscore_all(
    zscore = data_frame_zscore,
    select_method = "algA",
    place = "UI"
  )

 expect_equal(names(plot_1$labels), c("x", "y", "title"))
 expect_equal(unname(unlist(plot_1$labels)), c("Laboratory",  "Z-Score", "Z-Score plot. Method: algA"))


  plot_2 <- plot_deviation_all(
    zscore = data_frame_zscore,
    select_method = "algA",
    place = "UI"
  )

  expect_equal(names(plot_2$labels), c("x", "y", "title"))
  expect_equal(unname(unlist(plot_2$labels)), c("Laboratory",  "Deviation from reference dose [Gy]",  "Deviation from reference dose"))

  plot_3 <- plot_interlab_v2(
    zscore = data_frame_zscore,
    select_method = "algA",
    sum_table = data_frame_zscore,
    place = "UI"
  )


  for(i in 1:3){
    expect_equal(names(plot_3[[i]]$labels), c( "x", "y", "title"))
    expect_equal(unname(unlist(plot_3[[i]]$labels)), c("Laboratory", "Z-Score", paste("Z-Score plot. Method: algA. Sample:", i)))
  }

  plot_4 <- plot_interlab_deviation(
    zscore = data_frame_zscore,
    sum_table = data_frame_zscore,
    place = "UI"
  )


  for(i in 1:3){
    expect_equal(names(plot_4[[i]]$labels), c( "x", "y", "title"))
    expect_equal(unname(unlist(plot_4[[i]]$labels)), c("Laboratory", "Deviaition from reference [Gy]", paste("Deviation from reference dose Sample:", i)
                                                       ))
  }


  line_triage <- list(`1` = 2.56, `2` = 3.41, `3` = 4.54)

  plot_5 <- plot_triage_interlab(
    line_triage,
    sum_table = tables_list[[1]],
    place = "UI"
  )

  for(i in 1:3){
    expect_equal(names(plot_5[[i]]$labels), c( "x", "y", "title" ))
    expect_equal(unname(unlist(plot_5[[i]]$labels)), c("Laboratories",  "Dose (Gy)", paste("Dose estimation plot. Sample: ", i)
                                                       ))
  }

  # Test Z-score calculation for frequencies

  zscore_S1_algA <- calc.zValue.new(
    X =  tables_list[[1]][["y"]][1:2],
    type = "frequency",
    alg = "algA", NA)

  zscore_S1_algB <- calc.zValue.new(
    X =  tables_list[[1]][["y"]][1:2],
    type = "frequency",
    alg = "algB", NA)

  zscore_S1_QHampel <- calc.zValue.new(
    X =  tables_list[[1]][["y"]][1:2],
    type = "frequency",
    alg = "QHampel", NA)


  expect_equal(round(zscore_S1_algA, 4), c(0.4675, -0.4675))
  expect_equal(round(zscore_S1_algB, 4), c(0.4939, -0.4939))
  expect_equal(round(zscore_S1_QHampel, 4), c(0.3376, -0.3376))


  zscore_S2_algA <- calc.zValue.new(
    X =  tables_list[[1]][["y"]][3:4],
    type = "frequency",
    alg = "algA", NA)

  zscore_S2_algB <- calc.zValue.new(
    X =  tables_list[[1]][["y"]][3:4],
    type = "frequency",
    alg = "algB", NA)

  zscore_S2_QHampel <- calc.zValue.new(
    X =  tables_list[[1]][["y"]][3:4],
    type = "frequency",
    alg = "QHampel", NA)


  expect_equal(round(zscore_S2_algA, 4), c(0.4675, -0.4675))
  expect_equal(round(zscore_S2_algB, 4), c(0.4939, -0.4939))
  expect_equal(round(zscore_S2_QHampel, 4), c(0.3376, -0.3376))



  zscore_S3_algA <- calc.zValue.new(
    X =  tables_list[[1]][["y"]][5:6],
    type = "frequency",
    alg = "algA", NA)

  zscore_S3_algB <- calc.zValue.new(
    X =  tables_list[[1]][["y"]][5:6],
    type = "frequency",
    alg = "algB", NA)

  zscore_S3_QHampel <- calc.zValue.new(
    X =  tables_list[[1]][["y"]][5:6],
    type = "frequency",
    alg = "QHampel", NA)


  expect_equal(round(zscore_S3_algA, 4), c(0.4675, -0.4675))
  expect_equal(round(zscore_S3_algB, 4), c(0.4939, -0.4939))
  expect_equal(round(zscore_S3_QHampel, 4), c(0.3376, -0.3376))


  data_frame_zscore <- data.frame(
    Lab = tables_list[[1]][["Lab"]],
    Sample = tables_list[[1]][["Sample"]],
    Type = tables_list[[1]][["Type"]],
    Frequency=tables_list[[1]][["y"]],
    #Deviation = rep(NULL, 6),
    Zscore = c(zscore_S1_algA, zscore_S2_algA, zscore_S3_algA),
    stringsAsFactors = FALSE
  )

  expect_equal(colnames(data_frame_zscore), c("Lab", "Sample", "Type", "Frequency", "Zscore"))
  expect_equal(data_frame_zscore[, "Lab"], rep(c("A1", "A2"), 3))
  expect_equal(data_frame_zscore[, "Sample"], rep(c("1", "2", "3"), rep(2, 3)))
  expect_equal(round(data_frame_zscore[, "Frequency"], 4), c(0.700, 0.555, 1.020, 0.735, 1.840, 1.265))
  expect_equal(round(data_frame_zscore[, "Zscore"], 4), c(0.4675, -0.4675,  0.4675, -0.4675,  0.4675, -0.4675))



  plot_1 <- plot_zscore_all(
    zscore = data_frame_zscore,
    select_method = "algA",
    place = "UI"
  )

  expect_equal(names(plot_1$labels), c("x", "y", "title"))
  expect_equal(unname(unlist(plot_1$labels)), c("Laboratory",  "Z-Score", "Z-Score plot. Method: algA"))


  plot_3 <- plot_interlab_v2(
    zscore = data_frame_zscore,
    select_method = "algA",
    sum_table = data_frame_zscore,
    place = "UI"
  )

  for(i in 1:3){
    expect_equal(names(plot_3[[i]]$labels), c( "x", "y", "title"))
    expect_equal(unname(unlist(plot_3[[i]]$labels)), c("Laboratory", "Z-Score", paste("Z-Score plot. Method: algA. Sample:", i)
                                                       ))
  }

})
