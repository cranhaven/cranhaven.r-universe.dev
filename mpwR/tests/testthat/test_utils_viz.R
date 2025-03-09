
#plot_CV_density
test_that("viz_CV_density works", {

   set.seed(123)
   data <- tibble::tibble(
      Analysis_mpwR = rep(c("A", "B"), each = 10),
      CV_Retention.time_mpwR = sample(1:30, 20),
      CV_Peptide_LFQ_mpwR = sample(1:30, 20),
      CV_ProteinGroup_LFQ_mpwR = sample(1:30, 20)
   )

   output <- viz_CV_density(input_df = data, cv_col = "CV_Retention.time_mpwR")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["CV_Retention.time_mpwR"]], c(15, 19, 14, 3, 10, 18, 22, 11, 5, 20, 28, 24, 9, 27, 8, 26, 7, 30, 25, 17))
   expect_equal(viz_CV_density(input_df = data[, -2], cv_col = "CV_Retention.time_mpwR"), NULL)

   output <- viz_CV_density(input_df = data, cv_col = "CV_Peptide_LFQ_mpwR")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["CV_Peptide_LFQ_mpwR"]], c(4, 14, 17, 11, 7, 21, 12, 15, 10, 13, 26, 9, 19, 22, 20, 27, 24, 5, 16, 25))
   expect_equal(viz_CV_density(input_df = data[, -3], cv_col = "CV_Peptide_LFQ_mpwR"), NULL)

   output <- viz_CV_density(input_df = data, cv_col = "CV_ProteinGroup_LFQ_mpwR")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["CV_ProteinGroup_LFQ_mpwR"]], c(27, 6, 25, 2, 5, 8, 12, 13, 18, 1, 29, 15, 9, 19, 10, 24, 4, 20, 11, 28))
   expect_equal(viz_CV_density(input_df = data[, -4], cv_col = "CV_ProteinGroup_LFQ_mpwR"), NULL)

})

#viz_DC_barplot
test_that("viz_DC_barplot works", {

   set.seed(123)
   data <- tibble::tibble(
      Nr.Missing.Values = c(0, 1, 2, 3, 4),
      Precursor.IDs = sample(1:100, 5),
      Peptide.IDs = sample(1:100, 5),
      Protein.IDs = sample(1:100, 5),
      ProteinGroup.IDs = sample(1:100, 5)
   )

   output <- viz_DC_barplot(input_df = data, level = "Precursor.IDs", label = "absolute")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["input_df"]][["Nr.Missing.Values"]], c(0, 1, 2, 3, 4))

   output <- viz_DC_barplot(input_df = data, level = "Precursor.IDs", label = "percentage")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["input_df"]][["Nr.Missing.Values"]], c(0, 1, 2, 3, 4))

   output <- viz_DC_barplot(input_df = data, level = "Peptide.IDs", label = "absolute")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["input_df"]][["Nr.Missing.Values"]], c(0, 1, 2, 3, 4))

   output <- viz_DC_barplot(input_df = data, level = "Peptide.IDs", label = "percentage")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["input_df"]][["Nr.Missing.Values"]], c(0, 1, 2, 3, 4))

   output <- viz_DC_barplot(input_df = data, level = "Protein.IDs", label = "absolute")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["input_df"]][["Nr.Missing.Values"]], c(0, 1, 2, 3, 4))

   output <- viz_DC_barplot(input_df = data, level = "Protein.IDs", label = "percentage")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["input_df"]][["Nr.Missing.Values"]], c(0, 1, 2, 3, 4))

   output <- viz_DC_barplot(input_df = data, level = "ProteinGroup.IDs", label = "absolute")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["input_df"]][["Nr.Missing.Values"]], c(0, 1, 2, 3, 4))

   output <- viz_DC_barplot(input_df = data, level = "ProteinGroup.IDs", label = "percentage")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["input_df"]][["Nr.Missing.Values"]], c(0, 1, 2, 3, 4))

   #Spectronaut
   data <- tibble::tibble(
      Nr.Missing.Values = c(0, 1, 2, 3, 4),
      Protein.IDs = c(NA, NA, NA, NA, NA)
   )
   expect_equal(viz_DC_barplot(input_df = data, level = "Protein.IDs", label = "absolute"), NULL)

})

#viz_DC_stacked_barplot
test_that("viz_DC_stacked_barplot works", {

   set.seed(134)
   data <- tibble::tibble(
      Analysis = rep(c("A", "B"), each = 4),
      Profile = rep(c("unique", "sparse", "shared with at least 50%", "complete"), times = 2),
      Precursor.IDs = sample(1:100, 8),
      Peptide.IDs = sample(1:100, 8),
      Protein.IDs = sample(1:100, 8),
      ProteinGroup.IDs = sample(1:100, 8)
   )

   output <- viz_DC_stacked_barplot(input_df = data, level = "Precursor.IDs", label = "absolute")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["Analysis"]], factor(rep(c("A","B"), each = 4), level = c("B", "A")))

   output <- viz_DC_stacked_barplot(input_df = data, level = "Precursor.IDs", label = "percentage")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["Analysis"]], factor(rep(c("A","B"), each = 4), level = c("B", "A")))

   output <- viz_DC_stacked_barplot(input_df = data, level = "Peptide.IDs", label = "absolute")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["Analysis"]], factor(rep(c("A","B"), each = 4), level = c("B", "A")))

   output <- viz_DC_stacked_barplot(input_df = data, level = "Peptide.IDs", label = "percentage")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["Analysis"]], factor(rep(c("A","B"), each = 4), level = c("B", "A")))

   output <- viz_DC_stacked_barplot(input_df = data, level = "Protein.IDs", label = "absolute")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["Analysis"]], factor(rep(c("A","B"), each = 4), level = c("B", "A")))

   output <- viz_DC_stacked_barplot(input_df = data, level = "Protein.IDs", label = "percentage")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["Analysis"]], factor(rep(c("A","B"), each = 4), level = c("B", "A")))

   output <- viz_DC_stacked_barplot(input_df = data, level = "ProteinGroup.IDs", label = "absolute")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["Analysis"]], factor(rep(c("A","B"), each = 4), level = c("B", "A")))

   output <- viz_DC_stacked_barplot(input_df = data, level = "ProteinGroup.IDs", label = "percentage")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["Analysis"]], factor(rep(c("A","B"), each = 4), level = c("B", "A")))

   #reduce profile options
   set.seed(134)
   data <- tibble::tibble(
      Analysis = rep(c("A", "B"), each = 3),
      Profile = rep(c("unique", "sparse", "shared with at least 50%"), times = 2),
      Precursor.IDs = sample(1:100, 6),
      Peptide.IDs = sample(1:100, 6),
      Protein.IDs = sample(1:100, 6),
      ProteinGroup.IDs = sample(1:100, 6)
   )

   output <- viz_DC_stacked_barplot(input_df = data, level = "Precursor.IDs", label = "absolute")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["Analysis"]], factor(rep(c("A","B"), each = 3), level = c("B", "A")))

   #Spectronaut
   data <- tibble::tibble(
      Analysis = rep(c("A", "B"), each = 3),
      Profile = rep(c("unique", "sparse", "shared with at least 50%"), times = 2),
      Protein.IDs = c(NA, NA, NA, NA, NA, NA)
   )

   expect_equal(viz_DC_stacked_barplot(input_df = data, level = "Protein.IDs", label = "absolute"), NULL)

})

#viz_ID_barplot
test_that("viz_ID_barplot works", {

   set.seed(123)
   data <- tibble::tibble(
      Run = c("R01", "R02", "R03"),
      ProteinGroup.IDs = sample(1:100, 3),
      Protein.IDs = sample(1:100, 3),
      Peptide.IDs = sample(1:100, 3),
      Precursor.IDs = sample(1:100, 3)
   )

   output <- viz_ID_barplot(input_df = data, level = "Precursor.IDs")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["Run"]], factor(c("R01", "R02", "R03"), level = c("R03", "R02", "R01")))

   output <- viz_ID_barplot(input_df = data, level = "Peptide.IDs")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["Run"]], factor(c("R01", "R02", "R03"), level = c("R03", "R02", "R01")))

   output <- viz_ID_barplot(input_df = data, level = "Protein.IDs")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["Run"]], factor(c("R01", "R02", "R03"), level = c("R03", "R02", "R01")))

   output <- viz_ID_barplot(input_df = data, level = "ProteinGroup.IDs")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["plot_env"]][["."]][["Run"]], factor(c("R01", "R02", "R03"), level = c("R03", "R02", "R01")))

   #Spectronaut
   data <- tibble::tibble(
      Run = c("R01", "R02", "R03"),
      Protein.IDs = c(NA, NA, NA)
   )

   expect_equal(viz_ID_barplot(input_df = data, level = "Protein.IDs"), NULL)

})

#viz_ID_boxplot
test_that("viz_ID_boxplot works", {

   set.seed(123)
   data <- tibble::tibble(
      Analysis = rep(c("A", "B"), each = 10),
      Precursor.IDs = sample(1:100, 20),
      Peptide.IDs = sample(1:100, 20),
      Protein.IDs = sample(1:100, 20),
      ProteinGroup.IDs = sample(1:100, 20)
   )

   output <- viz_ID_boxplot(input_df = data, level = "Precursor.IDs")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["labels"]][["x"]], "Analysis \n")

   output <- viz_ID_boxplot(input_df = data, level = "Peptide.IDs")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["labels"]][["x"]], "Analysis \n")

   output <- viz_ID_boxplot(input_df = data, level = "Protein.IDs")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["labels"]][["x"]], "Analysis \n")

   output <- viz_ID_boxplot(input_df = data, level = "ProteinGroup.IDs")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["labels"]][["x"]], "Analysis \n")

   #Spectronaut
   data <- tibble::tibble(
      Analysis = rep(c("A", "B"), each = 2),
      Protein.IDs = c(NA, NA, NA, NA)
   )

   expect_equal(viz_ID_boxplot(input_df = data, level = "Protein.IDs"), NULL)

})

#viz_MC_barplot
test_that("viz_MC_barplot works", {

   data <- tibble::tibble(
      Analysis = c("A", "A", "A", "A", "A"),
      Missed.Cleavage = c("0", "1", "2", "3", "No R/K cleavage site"),
      mc_count = c("2513", "368", "23", "38", "10")
   )

   output <- viz_MC_barplot(input_df = data, label = "absolute")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["data"]][["mc_count"]], c(2513, 368, 23, 38, 10))

   output <- viz_MC_barplot(input_df = data, label = "percentage")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["data"]][["mc_count"]], c(2513, 368, 23, 38, 10))

})

#viz_MC_stacked_barplot
test_that("viz_MC_stacked_barplot works", {

   data <- tibble::tibble(
      Analysis = rep(c("A", "B"), each = 5),
      Missed.Cleavage = c("0", "1", "2", "3", "No R/K cleavage site", "0", "1", "2", "3", "No R/K cleavage site"),
      mc_count = c("2513", "368", "23", "38", "10", "2513", "368", "23", "38", "10")
   )

   output <- viz_MC_stacked_barplot(input_df = data, label = "absolute")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["data"]][["Peptide_count"]], c(2513, 368, 23, 38, 10, 2513, 368, 23, 38, 10))

   output <- viz_MC_stacked_barplot(input_df = data, label = "percentage")

   expect_s3_class(output, "ggplot")
   expect_equal(output[["data"]][["Peptide_count"]], c(2513, 368, 23, 38, 10, 2513, 368, 23, 38, 10))

   #check colors
   data <- tibble::tibble(
      Analysis = c("A", "A", "A", "B", "B", "B"),
      Missed.Cleavage = c("0", "1", "2", "0", "1", "2"),
      mc_count = c("2513", "368", "10", "2513", "368", "10")
   )

   output <- viz_MC_stacked_barplot(input_df = data, label = "absolute")
   expect_s3_class(output, "ggplot")

   data <- tibble::tibble(
      Analysis = c("A", "A", "B", "B"),
      Missed.Cleavage = c("0", "1", "0", "1"),
      mc_count = c("2513", "368", "2513", "368")
   )

   output <- viz_MC_stacked_barplot(input_df = data, label = "absolute")
   expect_s3_class(output, "ggplot")

   data <- tibble::tibble(
      Analysis = c("A",  "B"),
      Missed.Cleavage = c("0", "0"),
      mc_count = c("2513", "2513")
   )

   output <- viz_MC_stacked_barplot(input_df = data, label = "absolute")
   expect_s3_class(output, "ggplot")

})
