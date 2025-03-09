
test_that("calculate_CV works", {

  data_RT <- tibble::tibble(
    "Run_mpwR" = c("R01", "R01", "R02", "R03", "R01"),
    "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "A1", "B2"), #same precursor multiple times per run possible - first RT entry used
    "Retention.time_mpwR" = c(3, 3.5, 4, 5, 4)
  )

 data_pepLFQ <- tibble::tibble(
   "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02"),
   "Stripped.Sequence_mpwR" = c("A1", "A1", "A1", "B2", "B2"),
   "Peptide_LFQ_mpwR" = c(3, 4, 5, 4, NA)
 )

 data_pgLFQ <- tibble::tibble(
   "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02"),
   "ProteinGroup.IDs_mpwR" = c("A1", "A1", "A1", "B2", "B2"),
   "ProteinGroup_LFQ_mpwR" = c(3, 4, 5, 4, 4)
 )

 output_RT <- calculate_CV(input_df = data_RT, cv_col = "Retention.time", analysis_name = "test")
 output_pepLFQ <- calculate_CV(input_df = data_pepLFQ, cv_col = "Peptide_LFQ", analysis_name = "test")
 output_pgLFQ <- calculate_CV(input_df = data_pgLFQ, cv_col = "ProteinGroup_LFQ", analysis_name = "test")

 expect_s3_class(output_RT, "tbl")
 expect_equal(nrow(output_RT), 1)
 expect_equal(ncol(output_RT), 5)
 expect_equal(output_RT$CV_Retention.time_mpwR , 25)
 expect_equal(output_RT$Analysis_mpwR , "test")

 expect_s3_class(output_pepLFQ, "tbl")
 expect_equal(nrow(output_pepLFQ), 1)
 expect_equal(ncol(output_pepLFQ), 5)
 expect_equal(output_pepLFQ$CV_Peptide_LFQ_mpwR , 25)
 expect_equal(output_pepLFQ$Analysis_mpwR , "test")

 expect_s3_class(output_pgLFQ, "tbl")
 expect_equal(nrow(output_pgLFQ), 1)
 expect_equal(ncol(output_pgLFQ), 5)
 expect_equal(output_pgLFQ$CV_ProteinGroup_LFQ_mpwR , 25)
 expect_equal(output_pgLFQ$Analysis_mpwR , "test")

})

#add_analysis_col
test_that("add_analysis_col works", {

   data <- tibble::tibble(
      test_col = c("A", "B", "C")
   )

   output <- add_analysis_col(input_df = data, analysis_name = "test")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 2)
   expect_equal(output$Analysis , c("test", "test", "test"))

})

#generate_level_count
test_that("generate_level_count works", {

   data <- tibble::tibble(
      "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
      "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "B2", "B2", "B2", "C2", "C2"),
      "Peptide.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C"),
      "Protein.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C"),
      "ProteinGroup.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C")
   )

   output <- generate_level_count(input_df = data, level = "ProteinGroup.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 2)
   expect_equal(output$ProteinGroup.IDs , c(2, 3, 3))

   output <- generate_level_count(input_df = data, level = "Protein.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 2)
   expect_equal(output$Protein.IDs , c(2, 3, 3))

   output <- generate_level_count(input_df = data, level = "Peptide.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 2)
   expect_equal(output$Peptide.IDs , c(2, 3, 3))

   output <- generate_level_count(input_df = data, level = "Precursor.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 2)
   expect_equal(output$Precursor.IDs , c(2, 3, 3))
})

#tidy_MQ_pep_pg
test_that("tidy_MQ_pep_pg works", {

   data <- tibble::tibble(
         "Intensity 01" = c(4, 4, 3.9),
         "Intensity 02" = c(3, 3.5, NA),
         "Intensity 03" = c(3, 4, 0),
         "LFQ Intensity 01" = c(4, 4, 3.9),
         "LFQ Intensity 02" = c(3, 3.5, NA),
         "LFQ Intensity 03" = c(3, 4, 0)
      )

   output <- tidy_MQ_pep_pg(input_df = data)

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 7)
   expect_equal(ncol(output), 5)
   expect_equal(output$Run_mpwR , c("Intensity 01", "Intensity 02", "Intensity 03", "Intensity 01", "Intensity 02", "Intensity 03", "Intensity 01"))

})

#generate_DC_count
test_that("generate_DC_count works", {

   data <- tibble::tibble(
      "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
      "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "B2", "B2", "B2", "C2", "C2"),
      "Peptide.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C"),
      "Protein.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C"),
      "ProteinGroup.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C")
   )

   output <- generate_DC_count(input_df = data, level = "ProteinGroup.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 2)
   expect_equal(ncol(output), 2)
   expect_equal(output$ProteinGroup.IDs , c(1, 2))
   expect_equal(output$Nr_Appearances , c(2, 3))

   output <- generate_DC_count(input_df = data, level = "Protein.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 2)
   expect_equal(ncol(output), 2)
   expect_equal(output$Protein.IDs , c(1, 2))
   expect_equal(output$Nr_Appearances , c(2, 3))

   output <- generate_DC_count(input_df = data, level = "Peptide.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 2)
   expect_equal(ncol(output), 2)
   expect_equal(output$Peptide.IDs , c(1, 2))
   expect_equal(output$Nr_Appearances , c(2, 3))

   output <- generate_DC_count(input_df = data, level = "Precursor.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 2)
   expect_equal(ncol(output), 2)
   expect_equal(output$Precursor.IDs , c(1, 2))
   expect_equal(output$Nr_Appearances , c(2, 3))

})

#make_frame_DC
test_that("make_frame_DC works", {

   data <- tibble::tibble(
      Run_mpwR = c("R01", "R02", "R03", "R01")
   )

   output <- make_frame_DC(input_df = data)

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 2)
   expect_equal(output$Nr.Missing.Values, c(2, 1, 0))

})

#join_DC_levels
test_that("join_DC_levels works", {

   input_df <- tibble::tibble(Nr.Missing.Values = c(2, 1, 0),
                              Nr_Appearances = c(1, 2, 3))

   prec_df <- tibble::tibble(Precursor.IDs = c(2, 3),
                             Nr_Appearances = c(2, 3))

   pep_df <- tibble::tibble(Peptide.IDs = c(5, 2, 3),
                             Nr_Appearances = c(1, 2, 3))

   prot_df <- tibble::tibble(Protein.IDs = c(2, 3),
                             Nr_Appearances = c(1, 3))

   pg_df <- tibble::tibble(ProteinGroup.IDs = c(2, 3),
                             Nr_Appearances = c(2, 3))

   output <- join_DC_levels(input_df = input_df, prec_df = prec_df, pep_df = pep_df, prot_df = prot_df, pg_df = pg_df)

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 5)
   expect_equal(output$Precursor.IDs , c(0, 2, 3))
   expect_equal(output$Peptide.IDs , c(5, 2, 3))
   expect_equal(output$Protein.IDs , c(2, 0, 3))
   expect_equal(output$ProteinGroup.IDs , c(0, 2, 3))

})

#generate_DC_Report_percentage
test_that("generate_DC_Report_percentage works", {

   data <- tibble::tibble(
      Nr.Missing.Values = c(2, 1, 0),
      Precursor.IDs = c(5, 5, 10),
      Peptide.IDs = c(5, 5, 10),
      Protein.IDs = c(5, 5, 10),
      ProteinGroup.IDs = c(5, 5, 10)
   )

   output <- generate_DC_Report_percentage(input_df = data)

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 5)
   expect_equal(output$Precursor.IDs , c(25, 25, 50))
   expect_equal(output$Peptide.IDs , c(25, 25, 50))
   expect_equal(output$Protein.IDs , c(25, 25, 50))
   expect_equal(output$ProteinGroup.IDs , c(25, 25, 50))

})

#generate_DC_Report_profile
test_that("generate_DC_Report_profile works", {

   data <- tibble::tibble(
      Nr.Missing.Values = c(2, 1, 0),
      Precursor.IDs = c(0, 33, 67),
      Peptide.IDs = c(0, 33, 67),
      Protein.IDs = c(0, 33, 67),
      ProteinGroup.IDs = c(0, 33, 67)
   )

   output <- generate_DC_Report_profile(input_df = data)

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 6)
   expect_equal(output$Precursor.IDs , c(0, 33, 67))
   expect_equal(output$Profile , c("unique", "shared with at least 50%", "complete"))

})

#get_full_profile
test_that("get_full_profile works", {

   data <- tibble::tibble(
      "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
      "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "B2", "B2", "B2", "C2", "C2"),
      "Peptide.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C"),
      "Protein.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C"),
      "ProteinGroup.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C")
   )

   output <- get_full_profile(input_df = data, level = "ProteinGroup.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 2)
   expect_equal(ncol(output), 2)
   expect_equal(output$Nr_Appearances , c(3, 3))

   output <- get_full_profile(input_df = data, level = "Protein.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 2)
   expect_equal(ncol(output), 2)
   expect_equal(output$Nr_Appearances , c(3, 3))

   output <- get_full_profile(input_df = data, level = "Peptide.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 2)
   expect_equal(ncol(output), 2)
   expect_equal(output$Nr_Appearances , c(3, 3))

   output <- get_full_profile(input_df = data, level = "Precursor.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 2)
   expect_equal(ncol(output), 2)
   expect_equal(output$Nr_Appearances , c(3, 3))

})

#tidy_MQ_LFQ
test_that("tidy_MQ_LFQ works", {

   MQ_pep <- tibble::tibble(
      "Stripped.Sequence_mpwR" = c("A", "B", "C"),
      "Intensity 01" = c(4, 4, 3.9),
      "Intensity 02" = c(3, 3.5, 4),
      "Intensity 03" = c(3, 3.5, 4),
      "LFQ Intensity 01" = c(4, 4, 0),
      "LFQ Intensity 02" = c(3, 3.5, NA),
      "LFQ Intensity 03" = c(3, 3.5, 4)
   )

   MQ_pg <- tibble::tibble(
      "ProteinGroup.IDs_mpwR" = c("A", "B", "C"),
      "Intensity 01" = c(4, 4, 3.9),
      "Intensity 02" = c(3, 3.5, 4),
      "Intensity 03" = c(3, 3.5, 4),
      "LFQ Intensity 01" = c(4, 4, 0),
      "LFQ Intensity 02" = c(3, 3.5, NA),
      "LFQ Intensity 03" = c(3, 3.5, 4)
   )

   output <- tidy_MQ_LFQ(input_df = MQ_pep, cv_col = "Peptide_LFQ")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 7)
   expect_equal(ncol(output), 6)
   expect_equal(output$Run_mpwR , c("LFQ Intensity 01", "LFQ Intensity 02", "LFQ Intensity 03", "LFQ Intensity 01", "LFQ Intensity 02", "LFQ Intensity 03", "LFQ Intensity 03"))

   output <- tidy_MQ_LFQ(input_df = MQ_pg, cv_col = "ProteinGroup_LFQ")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 7)
   expect_equal(ncol(output), 6)
   expect_equal(output$Run_mpwR , c("LFQ Intensity 01", "LFQ Intensity 02", "LFQ Intensity 03", "LFQ Intensity 01", "LFQ Intensity 02", "LFQ Intensity 03", "LFQ Intensity 03"))

})

#generate_MC_Report_percentage
test_that("generate_MC_Report_percentage works", {

   data <- tibble::tibble(
      mc_count = c("1", "2", "1")
   )

   output <- generate_MC_Report_percentage(input_df = data)

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 1)
   expect_equal(output$mc_count , c(25, 50, 25))

})

#prepare_stacked_barplot
test_that("prepare_stacked_barplot works", {

   data <- tibble::tibble(
      Analysis = c("A", "A", "A", "B", "B"),
      Precursor.IDs = c(5, 5, 7, 10, 10),
      Peptide.IDs = c(5, 5, 7, 10, 10),
      Protein.IDs = c(5, 5, 7, 10, 10),
      ProteinGroup.IDs = c(5, 5, 7, 10, 10),
      Profile = c("unique", "shared with at least 50%", "shared with at least 50%", "unique", "complete")
   )

   output <- prepare_stacked_barplot(input_df = data, level = "Precursor.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 4)
   expect_equal(ncol(output), 3)
   expect_equal(output$Precursor.IDs , c(12, 5, 10, 10))
   expect_equal(output$Profile, c("shared with at least 50%", "unique", "complete", "unique"))

   output <- prepare_stacked_barplot(input_df = data, level = "Peptide.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 4)
   expect_equal(ncol(output), 3)
   expect_equal(output$Peptide.IDs , c(12, 5, 10, 10))
   expect_equal(output$Profile, c("shared with at least 50%", "unique", "complete", "unique"))

   output <- prepare_stacked_barplot(input_df = data, level = "Protein.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 4)
   expect_equal(ncol(output), 3)
   expect_equal(output$Protein.IDs , c(12, 5, 10, 10))
   expect_equal(output$Profile, c("shared with at least 50%", "unique", "complete", "unique"))

   output <- prepare_stacked_barplot(input_df = data, level = "ProteinGroup.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 4)
   expect_equal(ncol(output), 3)
   expect_equal(output$ProteinGroup.IDs , c(12, 5, 10, 10))
   expect_equal(output$Profile, c("shared with at least 50%", "unique", "complete", "unique"))

})

#get_median
test_that("get_median works", {

   data <- tibble::tibble(
      Analysis = c("A", "A", "A"),
      Precursor.IDs = c(5, 5, 7),
      Peptide.IDs = c(5, 5, 7),
      Protein.IDs = c(5, 5, 7),
      ProteinGroup.IDs = c(5, 5, 7)
   )

   output <- get_median(input_df = data, level = "Precursor.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 1)
   expect_equal(output$`Median Precursor.IDs [abs.]`, 5)

   output <- get_median(input_df = data, level = "Peptide.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 1)
   expect_equal(output$`Median Peptide.IDs [abs.]`, 5)

   output <- get_median(input_df = data, level = "Protein.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 1)
   expect_equal(output$`Median Protein.IDs [abs.]`, 5)

   output <- get_median(input_df = data, level = "ProteinGroup.IDs")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 1)
   expect_equal(output$`Median ProteinGroup.IDs [abs.]`, 5)

})

#get_DC_summary
test_that("get_DC_summary works", {

   data <- tibble::tibble(
      Nr.Missing.Values = c(2, 1, 0),
      Precursor.IDs = c(5, 5, 7),
      Peptide.IDs = c(5, 5, 7),
      Protein.IDs = c(5, 5, 7),
      ProteinGroup.IDs = c(5, 5, 7)
   )

   output <- get_DC_summary(input_df = data, metric = "absolute")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 4)
   expect_equal(output$`Full profile - ProteinGroup.IDs [abs.]`, 7)

   output <- get_DC_summary(input_df = data, metric = "percentage")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 4)
   expect_equal(output$`Full profile - ProteinGroup.IDs [%]`, 7)

})

#get_CV_IDs
test_that("get_CV_IDs works", {

   data <- tibble::tibble(
     CV_Retention.time_mpwR = c(3, 3, 3, 4),
     CV_Peptide_LFQ_mpwR = c(1, 2, 3, 4),
     CV_ProteinGroup_LFQ_mpwR = c(1, 2, 3, 4)
   )

   output <- get_CV_IDs(input_df = data, th_hold = 3, cv_col = "CV_Retention.time_mpwR")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 1)
   expect_equal(output$`Precursor.IDs [abs.] with a CV Retention time < 3 [%]`, 0)

   output <- get_CV_IDs(input_df = data, th_hold = 3, cv_col = "CV_Peptide_LFQ_mpwR")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 1)
   expect_equal(output$`Peptide.IDs [abs.] with a CV LFQ < 3 [%]`, 2)

   output <- get_CV_IDs(input_df = data, th_hold = 3, cv_col = "CV_ProteinGroup_LFQ_mpwR")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 1)
   expect_equal(output$`Proteingroup.IDs [abs.] with a CV LFQ < 3 [%]`, 2)

   #If CV_Peptide/CV_ProteinGroup are not present #DIA-NN #PD
   data <- tibble::tibble(
      CV_Retention.time_mpwR = c(1, 2, 3, 4)
   )

   output <- get_CV_IDs(input_df = data, th_hold = 3, cv_col = "CV_Peptide_LFQ_mpwR")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 1)
   expect_equal(output$`Peptide.IDs [abs.] with a CV LFQ < 3 [%]`, NA)

   output <- get_CV_IDs(input_df = data, th_hold = 3, cv_col = "CV_ProteinGroup_LFQ_mpwR")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 1)
   expect_equal(output$`Proteingroup.IDs [abs.] with a CV LFQ < 3 [%]`, NA)

})

#get_mc_zero
test_that("get_mc_zero works", {

   data <- tibble::tibble(
      Missed.Cleavage = c(0, 1, 2),
      mc_count = c(5, 5, 7)
   )

   output <- get_mc_zero(input_df = data, metric = "absolute")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 1)
   expect_equal(output$`Peptide IDs with zero missed cleavages [abs.]`, 5)

   output <- get_mc_zero(input_df = data, metric = "percentage")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 1)
   expect_equal(output$`Peptide IDs with zero missed cleavages [%]`, 5)

})

#get_summary
test_that("get_summary works", {

   ID <- tibble::tibble(
      Analysis = c("A", "A", "A"),
      Precursor.IDs = c(5, 5, 7),
      Peptide.IDs = c(5, 5, 7),
      Protein.IDs = c(5, 5, 7),
      ProteinGroup.IDs = c(5, 5, 7)
   )

   DC <- tibble::tibble(
      Nr.Missing.Values = c(2, 1, 0),
      Precursor.IDs = c(5, 5, 7),
      Peptide.IDs = c(5, 5, 7),
      Protein.IDs = c(5, 5, 7),
      ProteinGroup.IDs = c(5, 5, 7)
   )

   CV <- tibble::tibble(
      CV_Retention.time_mpwR = c(3, 3, 3, 4),
      CV_Peptide_LFQ_mpwR = c(1, 2, 3, 21),
      CV_ProteinGroup_LFQ_mpwR = c(1, 2, 3, 20)
   )

   MC <- tibble::tibble(
      Missed.Cleavage = c(0, 1, 2),
      mc_count = c(5, 5, 7)
   )

   output <- get_summary(ID_Report = ID, DC_Report_abs = DC, DC_Report_perc = DC, CV_RT = CV, CV_LFQ_PG = CV, CV_LFQ_Pep = CV, MC_Report_abs = MC, MC_Report_perc = MC)

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 18)
   expect_equal(output$`Median ProteinGroup.IDs [abs.]`, 5)
   expect_equal(output$`Full profile - Protein.IDs [%]`, 7)
   expect_equal(output$`Precursor.IDs [abs.] with a CV Retention time < 5 [%]`, 4)
   expect_equal(output$`Peptide.IDs [abs.] with a CV LFQ < 20 [%]`, 3)
   expect_equal(output$`Peptide IDs with zero missed cleavages [%]`, 5)

   #remove LFQ Pep/PG #DIA-NN #PD
   CV <- tibble::tibble(
      CV_Retention.time_mpwR = c(3, 3, 3, 4)
   )

   output <- get_summary(ID_Report = ID, DC_Report_abs = DC, DC_Report_perc = DC, CV_RT = CV, CV_LFQ_PG = CV, CV_LFQ_Pep = CV, MC_Report_abs = MC, MC_Report_perc = MC)

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 18)
   expect_equal(output$`Proteingroup.IDs [abs.] with a CV LFQ < 20 [%]`, NA)
   expect_equal(output$`Peptide.IDs [abs.] with a CV LFQ < 20 [%]`, NA)

})

#get_summary_percentage
test_that("get_summary_percentage", {

   data <- tibble::tibble(
      Analysis = c("A", "B"),
      `Median ProteinGroup.IDs [abs.]` = c(5, 10),
      `Median Protein.IDs [abs.]` = c(5, 10),
      `Median Peptide.IDs [abs.]` = c(5, 10),
      `Median Precursor.IDs [abs.]` = c(5, 10),
      `Full profile - Precursor.IDs [abs.]` = c(5, 10),
      `Full profile - Peptide.IDs [abs.]` = c(5, 10),
      `Full profile - Protein.IDs [abs.]` = c(5, 10),
      `Full profile - ProteinGroup.IDs [abs.]` = c(5, 10),
      `Full profile - Precursor.IDs [%]` = c(5, 10),
      `Full profile - Peptide.IDs [%]` = c(5, 10),
      `Full profile - Protein.IDs [%]` = c(5, 10),
      `Full profile - ProteinGroup.IDs [%]` = c(5, 10),
      `Precursor.IDs [abs.] with a CV Retention time < 5 [%]` = c(5, 10),
      `Proteingroup.IDs [abs.] with a CV LFQ < 20 [%]` = c(NA, 10),
      `Peptide.IDs [abs.] with a CV LFQ < 20 [%]` = c(NA, 10),
      `Peptide IDs with zero missed cleavages [abs.]` = c(5, 10),
      `Peptide IDs with zero missed cleavages [%]` = c(5, 10)
   )

   output <- get_summary_percentage(input_df = data)

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 2)
   expect_equal(ncol(output), 18)
   expect_equal(output$`Median ProteinGroup.IDs [abs.]`, c(50, 100))
   expect_equal(output$`Full profile - Protein.IDs [%]`, c(50, 100))
   expect_equal(output$`Precursor.IDs [abs.] with a CV Retention time < 5 [%]`, c(50, 100))
   expect_equal(output$`Peptide.IDs [abs.] with a CV LFQ < 20 [%]`, c(0, 100))
   expect_equal(output$`Peptide IDs with zero missed cleavages [%]`, c(50, 100))

})

#categories
test_that("categories works", {

   data <- tibble::tibble(
      Analysis = 0,
      B = 0,
      C = 0
   )

   output <- categories(input_df = data)

   expect_vector(output)
   expect_equal(length(output), 3)
   expect_equal(output, c("B", "C", "B"))

})

#prepare_Upset
test_that("prepare_Upset works", {

   #MaxQuant
   data <- tibble::tibble(
      Run_mpwR = c("R01", "R01", "R01", "R02", "R02"),
      "Modified sequence" = c("_AACLLPK_", "ALTDM(Dummy_Modification)PQMK","_ALTDM(Oxidation (M))PQM(Oxidation (M))R_", "_AACLLPK_", "ALTDM(Dummy_Modification)PQMK"),
      Charge = c(2, 2, 3, 2, 2),
      Precursor.IDs_mpwR = c("_AACLLPK_2", "ALTDM(Dummy_Modification)PQMK2", "_ALTDM(Oxidation (M))PQM(Oxidation (M))R_2", "_AACLLPK_2", "ALTDM(Dummy_Modification)PQMK2"),
      Peptide.IDs_mpwR = c("_AACLLPK_", "ALTDM(Dummy_Modification)PQMK", "_ALTDM(Oxidation (M))PQM(Oxidation (M))R_", "_AACLLPK_", "ALTDM(Dummy_Modification)PQMK"),
      Protein.IDs_mpwR = c("A", "B", "C", "A", "B"),
      ProteinGroup.IDs_mpwR = c("A", "B", "C", "A", "B"),
      "Protein IDs" = c("A", "B", "C", "A", "B") #flowTraceR
   )

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 100, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "MaxQuant")
   expect_equal(output, c("ALTDM(Dummy_Modification)PQMK2", "_AACLLPK_2"))

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 50, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "MaxQuant")
   expect_equal(output, c("ALTDM(Dummy_Modification)PQMK2", "_AACLLPK_2", "_ALTDM(Oxidation (M))PQM(Oxidation (M))R_2"))

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 50, flowTraceR = TRUE, remove_traceR_unknownMods = FALSE, software = "MaxQuant")
   expect_equal(output, c("AACLLPK2", "ALTDM(DummyModification)PQMK2", "ALTDM(UniMod:35)PQM(UniMod:35)R3"))

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 50, flowTraceR = TRUE, remove_traceR_unknownMods = TRUE, software = "MaxQuant")
   expect_equal(output, c("AACLLPK2", "ALTDM(UniMod:35)PQM(UniMod:35)R3"))

   output <- prepare_Upset(input_df = data, level = "Peptide.IDs", percentage_runs = 50, flowTraceR = TRUE, remove_traceR_unknownMods = TRUE, software = "MaxQuant")
   expect_equal(output, c("AACLLPK", "ALTDM(UniMod:35)PQM(UniMod:35)R"))

   output <- prepare_Upset(input_df = data, level = "Protein.IDs", percentage_runs = 100, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "MaxQuant")
   expect_equal(output, c("A", "B"))

   expect_error(prepare_Upset(input_df = data, level = "Protein.IDs", percentage_runs = 100, flowTraceR = TRUE, remove_traceR_unknownMods = FALSE, software = "MaxQuant"), "flowTraceR not available for Protein.IDs level - put flowTraceR = FALSE")

   output <- prepare_Upset(input_df = data, level = "ProteinGroup.IDs", percentage_runs = 100, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "MaxQuant")
   expect_equal(output, c("A", "B"))

   output <- prepare_Upset(input_df = data, level = "ProteinGroup.IDs", percentage_runs = 100, flowTraceR = TRUE, remove_traceR_unknownMods = TRUE, software = "MaxQuant")
   expect_equal(output, c("A", "B"))

   #DIA-NN
   data <- tibble::tibble(
      Run_mpwR = c("R01", "R01", "R01", "R02", "R02"),
      "Protein.Group" = c("A", "B", "C", "A", "B"), #flowTraceR
      "Precursor.Id" = c("AAAATGTIFTFR2", "AAC(Dummy_Modification)LLPK2", "AEDTAVYYC(UniMod:4)AK2", "AAAATGTIFTFR2", "AAC(Dummy_Modification)LLPK2"), #flowTraceR
      Precursor.IDs_mpwR = c("AAAATGTIFTFR2", "AAC(Dummy_Modification)LLPK2", "AEDTAVYYC(UniMod:4)AK2", "AAAATGTIFTFR2", "AAC(Dummy_Modification)LLPK2"),
      Peptide.IDs_mpwR = c("AAAATGTIFTFR", "AAC(Dummy_Modification)LLPK", "AEDTAVYYC(UniMod:4)AK", "AAAATGTIFTFR", "AAC(Dummy_Modification)LLPK"),
      Protein.IDs_mpwR = c("A", "B", "C", "A", "B"),
      ProteinGroup.IDs_mpwR = c("A", "B", "C", "A", "B")
   )

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 100, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "DIA-NN")
   expect_equal(output, c("AAAATGTIFTFR2", "AAC(Dummy_Modification)LLPK2"))

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 50, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "DIA-NN")
   expect_equal(output, c("AAAATGTIFTFR2", "AAC(Dummy_Modification)LLPK2", "AEDTAVYYC(UniMod:4)AK2"))

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 50, flowTraceR = TRUE, remove_traceR_unknownMods = FALSE, software = "DIA-NN")
   expect_equal(output, c("AAAATGTIFTFR2", "AAC(Dummy_Modification)LLPK2", "AEDTAVYYC(UniMod:4)AK2"))

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 50, flowTraceR = TRUE, remove_traceR_unknownMods = TRUE, software = "DIA-NN")
   expect_equal(output, c("AAAATGTIFTFR2", "AEDTAVYYC(UniMod:4)AK2"))

   output <- prepare_Upset(input_df = data, level = "Peptide.IDs", percentage_runs = 50, flowTraceR = TRUE, remove_traceR_unknownMods = TRUE, software = "DIA-NN")
   expect_equal(output, c("AAAATGTIFTFR", "AEDTAVYYC(UniMod:4)AK"))

   output <- prepare_Upset(input_df = data, level = "Protein.IDs", percentage_runs = 100, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "DIA-NN")
   expect_equal(output, c("A", "B"))

   expect_error(prepare_Upset(input_df = data, level = "Protein.IDs", percentage_runs = 100, flowTraceR = TRUE, remove_traceR_unknownMods = FALSE, software = "DIA-NN"), "flowTraceR not available for Protein.IDs level - put flowTraceR = FALSE")

   output <- prepare_Upset(input_df = data, level = "ProteinGroup.IDs", percentage_runs = 100, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "DIA-NN")
   expect_equal(output, c("A", "B"))

   output <- prepare_Upset(input_df = data, level = "ProteinGroup.IDs", percentage_runs = 100, flowTraceR = TRUE, remove_traceR_unknownMods = TRUE, software = "DIA-NN")
   expect_equal(output, c("A", "B"))

   #Spectronaut
   data <- tibble::tibble(
      Run_mpwR = c("R01", "R01", "R01", "R02", "R02"),
      "PG.ProteinGroups" = c("A", "B", "C", "A", "B"), #flowTraceR
      "EG.PrecursorId" = c("_AEDYC[Carbamidomethyl (C)]AK_.2", "_M[Oxidation (M)]KPNFK_.2", "_M[Dummy_Modification (M)]KFK_.3", "_AEDYC[Carbamidomethyl (C)]AK_.2", "_M[Dummy_Modification (M)]KFK_.3"), #flowTraceR
      Precursor.IDs_mpwR = c("_AEDYC[Carbamidomethyl (C)]AK_.2", "_M[Oxidation (M)]KPNFK_.2", "_M[Dummy_Modification (M)]KFK_.3", "_AEDYC[Carbamidomethyl (C)]AK_.2", "_M[Dummy_Modification (M)]KFK_.3"),
      Peptide.IDs_mpwR = c("_AEDYC[Carbamidomethyl (C)]AK_", "_M[Oxidation (M)]KPNFK_", "_M[Dummy_Modification (M)]KFK_", "_AEDYC[Carbamidomethyl (C)]AK_", "_M[Dummy_Modification (M)]KFK_"),
      Protein.IDs_mpwR = c("A", "B", "C", "A", "B"),
      ProteinGroup.IDs_mpwR = c("A", "B", "C", "A", "B")
   )

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 100, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "Spectronaut")
   expect_equal(output, c("_AEDYC[Carbamidomethyl (C)]AK_.2", "_M[Dummy_Modification (M)]KFK_.3"))

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 50, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "Spectronaut")
   expect_equal(output, c("_AEDYC[Carbamidomethyl (C)]AK_.2", "_M[Dummy_Modification (M)]KFK_.3", "_M[Oxidation (M)]KPNFK_.2" ))

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 50, flowTraceR = TRUE, remove_traceR_unknownMods = FALSE, software = "Spectronaut")
   expect_equal(output, c("AEDYC(UniMod:4)AK2", "M(UniMod:35)KPNFK2", "M[DummyModification (M)]KFK3"))

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 50, flowTraceR = TRUE, remove_traceR_unknownMods = TRUE, software = "Spectronaut")
   expect_equal(output, c("AEDYC(UniMod:4)AK2", "M(UniMod:35)KPNFK2"))

   output <- prepare_Upset(input_df = data, level = "Peptide.IDs", percentage_runs = 50, flowTraceR = TRUE, remove_traceR_unknownMods = TRUE, software = "Spectronaut")
   expect_equal(output, c("AEDYC(UniMod:4)AK", "M(UniMod:35)KPNFK"))

   expect_error(prepare_Upset(input_df = data, level = "Protein.IDs", percentage_runs = 100, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "Spectronaut"), "Protein.IDs not available for Spectronaut")

   output <- prepare_Upset(input_df = data, level = "ProteinGroup.IDs", percentage_runs = 100, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "Spectronaut")
   expect_equal(output, c("A", "B"))

   output <- prepare_Upset(input_df = data, level = "ProteinGroup.IDs", percentage_runs = 100, flowTraceR = TRUE, remove_traceR_unknownMods = TRUE, software = "Spectronaut")
   expect_equal(output, c("A", "B"))

   #PD
   data <- tibble::tibble(
      Run_mpwR = c("R01", "R01", "R01", "R02", "R02"),
      "Protein Accessions" = c("A", "B", "C", "A", "B"), #flowTraceR
      "Annotated Sequence" = c("AAcLLPK", "AADDTWEPFASGK", "DYELLcLDGTRKPVEEYANcHLAR", "AAcLLPK", "AADDTWEPFASGK"), #flowTraceR
      Modifications = c("C3(Carbamidomethyl)", "", "C6(Carbamidomethyl); C20(Dummy_Modification)", "C3(Carbamidomethyl)", ""), #flowTraceR
      Charge = c(2, 2, 3, 2, 2), #flowTraceR
      Precursor.IDs_mpwR = c("AAcLLPK2", "AADDTWEPFASGK2", "DYELLcLDGTRKPVEEYANcHLAR3", "AAcLLPK2", "AADDTWEPFASGK2"),
      Peptide.IDs_mpwR = c("AAcLLPK", "AADDTWEPFASGK", "DYELLcLDGTRKPVEEYANcHLAR", "AAcLLPK", "AADDTWEPFASGK"),
      Protein.IDs_mpwR = c("A", "B", "C", "A", "B"),
      ProteinGroup.IDs_mpwR = c("A", "B", "C", "A", "B")
   )

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 100, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "PD")
   expect_equal(output, c("AADDTWEPFASGK2", "AAcLLPK2"))

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 50, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "PD")
   expect_equal(output, c("AADDTWEPFASGK2", "AAcLLPK2", "DYELLcLDGTRKPVEEYANcHLAR3"))

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 50, flowTraceR = TRUE, remove_traceR_unknownMods = FALSE, software = "PD")
   expect_equal(output, c("AAC(UniMod:4)LLPK2", "AADDTWEPFASGK2", "DYELLC(UniMod:4)LDGTRKPVEEYANC(Dummy_Modification)HLAR3"))

   output <- prepare_Upset(input_df = data, level = "Precursor.IDs", percentage_runs = 50, flowTraceR = TRUE, remove_traceR_unknownMods = TRUE, software = "PD")
   expect_equal(output, c("AAC(UniMod:4)LLPK2", "AADDTWEPFASGK2"))

   output <- prepare_Upset(input_df = data, level = "Peptide.IDs", percentage_runs = 50, flowTraceR = TRUE, remove_traceR_unknownMods = TRUE, software = "PD")
   expect_equal(output, c("AAC(UniMod:4)LLPK", "AADDTWEPFASGK"))

   output <- prepare_Upset(input_df = data, level = "Protein.IDs", percentage_runs = 100, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "PD")
   expect_equal(output, c("A", "B"))

   expect_error(prepare_Upset(input_df = data, level = "Protein.IDs", percentage_runs = 100, flowTraceR = TRUE, remove_traceR_unknownMods = FALSE, software = "PD"), "flowTraceR not available for Protein.IDs level - put flowTraceR = FALSE")

   output <- prepare_Upset(input_df = data, level = "ProteinGroup.IDs", percentage_runs = 100, flowTraceR = FALSE, remove_traceR_unknownMods = FALSE, software = "PD")
   expect_equal(output, c("A", "B"))

   output <- prepare_Upset(input_df = data, level = "ProteinGroup.IDs", percentage_runs = 100, flowTraceR = TRUE, remove_traceR_unknownMods = TRUE, software = "PD")
   expect_equal(output, c("A", "B"))

})

