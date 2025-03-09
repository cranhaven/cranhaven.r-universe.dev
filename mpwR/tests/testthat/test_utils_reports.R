
#generate DC Report
test_that("generate_DC_Report works", {

   MQ_ev <- tibble::tibble(
      "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
      "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "B2", "B2", "B2", "C2", "C2"),
      "Peptide.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C"),
      "Protein.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C")
   )

   MQ_pg <- tibble::tibble(
      "ProteinGroup.IDs_mpwR" = c("A", "B", "C"),
      "Intensity 01" = c(10, 10, NA),
      "Intensity 02" = c(3, 3.5, 4),
      "Intensity 03" = c(3, 3.5, 4)
   )

   output_MQ <- generate_DC_Report(input_df = MQ_ev, input_MQ_proteingroup = MQ_pg, analysis_name = "test", software = "MaxQuant", metric = "percentage")

   expect_s3_class(output_MQ, "data.frame")
   expect_equal(nrow(output_MQ), 3)
   expect_equal(ncol(output_MQ), 7)
   expect_equal(output_MQ$ProteinGroup.IDs , c(0.00, 33.33, 66.67))
   expect_equal(output_MQ$Precursor.IDs , c(0.00, 33.33, 66.67))
   expect_equal(output_MQ$Analysis , c("test", "test", "test"))

   PD_psm <- tibble::tibble(
      "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
      "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "B2", "B2", "B2", "C2", "C2"),
      "Peptide.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C")
   )

   PD_prot <- tibble::tibble(
      "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
      "Protein.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C")
   )

   PD_pg <- tibble::tibble(
      "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
      "ProteinGroup.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C") #only number in original data frame
   )

   output_PD <- generate_DC_Report(input_df = PD_psm, input_PD_protein = PD_prot, input_PD_proteingroup = PD_pg, analysis_name = "test", software = "PD", metric = "percentage")

   expect_s3_class(output_PD, "data.frame")
   expect_equal(nrow(output_PD), 3)
   expect_equal(ncol(output_PD), 7)
   expect_equal(output_PD$ProteinGroup.IDs , c(0.00, 33.33, 66.67))
   expect_equal(output_PD$Precursor.IDs , c(0.00, 33.33, 66.67))
   expect_equal(output_PD$Analysis , c("test", "test", "test"))

   #DIA-NN #Spectronaut #Generic
   input <- tibble::tibble(
      "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
      "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "B2", "B2", "B2", "C2", "C2"),
      "Peptide.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C"),
      "Protein.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C"),
      "ProteinGroup.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C")
   )

   output <- generate_DC_Report(input_df = input, analysis_name = "test", software = "DIA-NN", metric = "percentage")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 7)
   expect_equal(output$ProteinGroup.IDs , c(0.00, 33.33, 66.67))
   expect_equal(output$Precursor.IDs , c(0.00, 33.33, 66.67))
   expect_equal(output$Analysis , c("test", "test", "test"))

   output <- generate_DC_Report(input_df = input, analysis_name = "test", software = "Spectronaut", metric = "percentage")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 7)
   expect_equal(output$ProteinGroup.IDs , c(0.00, 33.33, 66.67))
   expect_equal(output$Precursor.IDs , c(0.00, 33.33, 66.67))
   expect_equal(output$Analysis , c("test", "test", "test"))

   output <- generate_DC_Report(input_df = input, analysis_name = "test", software = "Generic", metric = "percentage")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 7)
   expect_equal(output$ProteinGroup.IDs , c(0.00, 33.33, 66.67))
   expect_equal(output$Precursor.IDs , c(0.00, 33.33, 66.67))
   expect_equal(output$Analysis , c("test", "test", "test"))
})

#generate ID Report
test_that("generate_ID_Report works", {

   MQ_ev <- tibble::tibble(
      "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
      "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "B2", "B2", "B2", "C2", "C2"),
      "Peptide.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C"),
      "Protein.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C")
   )

   MQ_pg <- tibble::tibble(
      "ProteinGroup.IDs_mpwR" = c("A", "B", "C"),
      "Intensity 01" = c(10, 10, NA),
      "Intensity 02" = c(3, 3.5, 4),
      "Intensity 03" = c(3, 3.5, 4)
   )

   output_MQ <- generate_ID_Report(input_df = MQ_ev, input_MQ_proteingroup = MQ_pg, analysis_name = "test", software = "MaxQuant")

   expect_s3_class(output_MQ, "data.frame")
   expect_equal(nrow(output_MQ), 3)
   expect_equal(ncol(output_MQ), 6)
   expect_equal(output_MQ$ProteinGroup.IDs , c(2, 3, 3))
   expect_equal(output_MQ$Precursor.IDs , c(2, 3, 3))
   expect_equal(output_MQ$Analysis , c("test", "test", "test"))

   PD_psm <- tibble::tibble(
      "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
      "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "B2", "B2", "B2", "C2", "C2"),
      "Peptide.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C")
   )

   PD_prot <- tibble::tibble(
      "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
      "Protein.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C")
   )

   PD_pg <- tibble::tibble(
      "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
      "ProteinGroup.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C") #only number in original data frame
   )

   output_PD <- generate_ID_Report(input_df = PD_psm, input_PD_protein = PD_prot, input_PD_proteingroup = PD_pg, analysis_name = "test", software = "PD")

   expect_s3_class(output_PD, "data.frame")
   expect_equal(nrow(output_PD), 3)
   expect_equal(ncol(output_PD), 6)
   expect_equal(output_PD$ProteinGroup.IDs , c(2, 3, 3))
   expect_equal(output_PD$Precursor.IDs , c(2, 3, 3))
   expect_equal(output_PD$Analysis , c("test", "test", "test"))

   #DIA-NN #Spectronaut #Generic
   input <- tibble::tibble(
      "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
      "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "B2", "B2", "B2", "C2", "C2"),
      "Peptide.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C"),
      "Protein.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C"),
      "ProteinGroup.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C")
   )

   output <- generate_ID_Report(input_df = input, analysis_name = "test", software = "DIA-NN")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 6)
   expect_equal(output$ProteinGroup.IDs , c(2, 3, 3))
   expect_equal(output$Precursor.IDs , c(2, 3, 3))
   expect_equal(output$Analysis , c("test", "test", "test"))

   output <- generate_ID_Report(input_df = input, analysis_name = "test", software = "Spectronaut")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 6)
   expect_equal(output$ProteinGroup.IDs , c(2, 3, 3))
   expect_equal(output$Precursor.IDs , c(2, 3, 3))
   expect_equal(output$Analysis , c("test", "test", "test"))

   output <- generate_ID_Report(input_df = input, analysis_name = "test", software = "Generic")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 6)
   expect_equal(output$ProteinGroup.IDs , c(2, 3, 3))
   expect_equal(output$Precursor.IDs , c(2, 3, 3))
   expect_equal(output$Analysis , c("test", "test", "test"))

})

#generate MC Report
test_that("generate_MC_Report works", {

   #MaxQuant #PD #Spectronaut #Generic
   data <- tibble::tibble(
      "Stripped.Sequence_mpwR" = c("ABCR", "AKCR", "ABKCK", "ARKAR"),
      "Missed.Cleavage_mpwR" = c(0, 1, 1, 2),
   )

   output <- generate_MC_Report(input_df = data, analysis_name = "test", software = "MaxQuant", metric = "percentage")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 3)
   expect_equal(output$mc_count, c(25, 50, 25))
   expect_equal(output$Analysis, c("test", "test", "test"))

   output <- generate_MC_Report(input_df = data, analysis_name = "test", software = "Spectronaut", metric = "percentage")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 3)
   expect_equal(output$mc_count, c(25, 50, 25))
   expect_equal(output$Analysis, c("test", "test", "test"))

   output <- generate_MC_Report(input_df = data, analysis_name = "test", software = "PD", metric = "absolute")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 3)
   expect_equal(output$mc_count, c(1, 2, 1))
   expect_equal(output$Analysis, c("test", "test", "test"))

   output <- generate_MC_Report(input_df = data, analysis_name = "test", software = "Generic", metric = "percentage")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 3)
   expect_equal(output$mc_count, c(25, 50, 25))
   expect_equal(output$Analysis, c("test", "test", "test"))

   #DIA-NN
   data <- tibble::tibble(
      "Stripped.Sequence_mpwR" = c("ABCR", "AKCR", "ABKCK", "ARKAR", "ABCR") #duplicates removed
   )

   output <- generate_MC_Report(input_df = data, analysis_name = "test", software = "DIA-NN", metric = "percentage")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 3)
   expect_equal(output$mc_count, c(25, 50, 25))
   expect_equal(output$Analysis, c("test", "test", "test"))

})

test_that("generate_summary_Report works",{

   #DIA-NN
   diann <- tibble::tibble(
      "Run_mpwR" = c("R01", "R01", "R02", "R03", "R01"),
      "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "A1", "B2"), #same precursor multiple times per run possible - first RT entry used
      "Retention.time_mpwR" = c(3, 3.5, 4, 5, 4),
      "ProteinGroup_LFQ_mpwR" = c(3, 4, 5, 4, 4),
      "Peptide.IDs_mpwR" = c("A", "A", "A", "A", "B"),
      "Protein.IDs_mpwR" = c("A", "A", "A", "A", "B"),
      "ProteinGroup.IDs_mpwR" = c("A", "A", "A", "A", "B"),
      "Stripped.Sequence_mpwR" = c("ABCR", "AKCR", "ABKCK", "ARKAR", "ABCDR")
   )

   output <- generate_summary_Report(input_df = diann, analysis_name = "test", software = "DIA-NN")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 18)
   expect_equal(output$Analysis, "test")
   expect_equal(output$`Median Peptide.IDs [abs.]`, 1)
   expect_equal(output$`Full profile - Precursor.IDs [abs.]`, 1)
   expect_equal(output$`Full profile - Precursor.IDs [%]`, 50)
   expect_equal(output$`Precursor.IDs [abs.] with a CV Retention time < 5 [%]`, 0)
   expect_equal(output$`Proteingroup.IDs [abs.] with a CV LFQ < 20 [%]`, 0)
   expect_equal(is.na(output$`Peptide.IDs [abs.] with a CV LFQ < 20 [%]`), TRUE)
   expect_equal(output$`Peptide IDs with zero missed cleavages [%]`, 40)

   #Spectronaut
   spectronaut <- tibble::tibble(
      "Run_mpwR" = c("R01", "R01", "R02", "R03", "R01"),
      "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "A1", "B2"), #same precursor multiple times per run possible - first RT entry used
      "Retention.time_mpwR" = c(3, 3.5, 4, 5, 4),
      "ProteinGroup_LFQ_mpwR" = c(3, 4, 5, 4, 4),
      "Peptide_LFQ_mpwR" = c(3, 4, 5, 4, NA),
      "Peptide.IDs_mpwR" = c("A", "A", "A", "A", "B"),
      "ProteinGroup.IDs_mpwR" = c("A", "A", "A", "A", "B"),
      "Stripped.Sequence_mpwR" = c("ABCR", "AKCR", "ABKCK", "ARKAR", "ABCDR"),
      "Missed.Cleavage_mpwR" = c(0, 1, 1, 2, 0),
   )

   output <- generate_summary_Report(input_df = spectronaut, analysis_name = "test", software = "Spectronaut")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 18)
   expect_equal(output$Analysis, "test")
   expect_equal(output$`Median Peptide.IDs [abs.]`, 1)
   expect_equal(output$`Full profile - Precursor.IDs [abs.]`, 1)
   expect_equal(output$`Full profile - Precursor.IDs [%]`, 50)
   expect_equal(output$`Precursor.IDs [abs.] with a CV Retention time < 5 [%]`, 0)
   expect_equal(output$`Proteingroup.IDs [abs.] with a CV LFQ < 20 [%]`, 0)
   expect_equal(is.na(output$`Median Protein.IDs [abs.]`), TRUE)
   expect_equal(is.na(output$`Full profile - Protein.IDs [abs.]`), TRUE)
   expect_equal(is.na(output$`Full profile - Protein.IDs [%]`), TRUE)
   expect_equal(output$`Peptide IDs with zero missed cleavages [%]`, 40)

   #MaxQuant
   MQ_ev <- tibble::tibble(
      "Run_mpwR" = c("R01", "R01", "R02", "R03", "R01"),
      "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "A1", "B2"), #same precursor multiple times per run possible - first RT entry used
      "Retention.time_mpwR" = c(3, 3.5, 4, 5, 4),
      "Peptide.IDs_mpwR" = c("A", "A", "A", "A", "B"),
      "Protein.IDs_mpwR" = c("A", "A", "A", "A", "B")
   )

   MQ_pep <- tibble::tibble(
      "Stripped.Sequence_mpwR" = c("A", "B", "C"),
      "Missed.Cleavage_mpwR" = c(0, 1, 2),
      "Intensity 01" = c(4, 4, 3.9),
      "Intensity 02" = c(3, 3.5, 4),
      "Intensity 03" = c(3, 3.5, 4),
      "LFQ Intensity 01" = c(4, 4, 3.9),
      "LFQ Intensity 02" = c(3, 3.5, 4),
      "LFQ Intensity 03" = c(3, 3.5, 4)
   )

   MQ_pg <- tibble::tibble(
      "ProteinGroup.IDs_mpwR" = c("A", "B", "C"),
      "Intensity 01" = c(4, 4, 3.9),
      "Intensity 02" = c(3, 3.5, 4),
      "Intensity 03" = c(3, 3.5, 4),
      "LFQ Intensity 01" = c(4, 4, 3.9),
      "LFQ Intensity 02" = c(3, 3.5, 4),
      "LFQ Intensity 03" = c(3, 3.5, 4)
   )

   output <- generate_summary_Report(input_df = MQ_ev, input_MQ_peptide = MQ_pep, input_MQ_proteingroup = MQ_pg, analysis_name = "test", software = "MaxQuant")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 18)
   expect_equal(output$Analysis, "test")
   expect_equal(output$`Median Peptide.IDs [abs.]`, 1)
   expect_equal(output$`Full profile - Precursor.IDs [abs.]`, 1)
   expect_equal(output$`Full profile - Precursor.IDs [%]`, 50)
   expect_equal(output$`Precursor.IDs [abs.] with a CV Retention time < 5 [%]`, 0)
   expect_equal(output$`Proteingroup.IDs [abs.] with a CV LFQ < 20 [%]`, 3)
   expect_equal(output$`Peptide IDs with zero missed cleavages [%]`, 33.33)

   #PD
   PD_psm <- tibble::tibble(
      "Run_mpwR" = c("R01", "R01", "R02", "R03", "R01"),
      "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "A1", "B2"), #same precursor multiple times per run possible - first RT entry used
      "Retention.time_mpwR" = c(3, 3.5, 4, 5, 4),
      "Peptide.IDs_mpwR" = c("A", "A", "A", "A", "B"),
      "Protein.IDs_mpwR" = c("A", "A", "A", "A", "B")
   )

   PD_prot <- tibble::tibble(
      "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
      "Protein.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C")
   )

   PD_pep <- tibble::tibble(
      "Stripped.Sequence_mpwR" = c("A", "B", "C"),
      "Missed.Cleavage_mpwR" = c(0, 1, 2)
   )

   PD_pg <- tibble::tibble(
      "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
      "ProteinGroup.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C") #only number in original data frame
   )

   output <- generate_summary_Report(input_df = PD_psm, input_PD_peptide = PD_pep, input_PD_protein = PD_prot, input_PD_proteingroup = PD_pg, analysis_name = "test", software = "PD")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 18)
   expect_equal(output$Analysis, "test")
   expect_equal(output$`Median Peptide.IDs [abs.]`, 1)
   expect_equal(output$`Full profile - Precursor.IDs [abs.]`, 1)
   expect_equal(output$`Full profile - Precursor.IDs [%]`, 50)
   expect_equal(output$`Precursor.IDs [abs.] with a CV Retention time < 5 [%]`, 0)
   expect_equal(output$`Proteingroup.IDs [abs.] with a CV LFQ < 20 [%]`, NA)
   expect_equal(output$`Peptide IDs with zero missed cleavages [%]`, 33.33)

   #Generic
   generic <- tibble::tibble(
     "Run_mpwR" = c("R01", "R01", "R02", "R03", "R01"),
     "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "A1", "B2"), #same precursor multiple times per run possible - first RT entry used
     "Retention.time_mpwR" = c(3, 3.5, 4, 5, 4),
     "ProteinGroup_LFQ_mpwR" = c(3, 4, 5, 4, 4),
     "Peptide_LFQ_mpwR" = c(3, 4, 5, 4, NA),
     "Peptide.IDs_mpwR" = c("A", "A", "A", "A", "B"),
     "Protein.IDs_mpwR" = c("A", "A", "A", "A", "B"),
     "ProteinGroup.IDs_mpwR" = c("A", "A", "A", "A", "B"),
     "Stripped.Sequence_mpwR" = c("ABCR", "AKCR", "ABKCK", "ARKAR", "ABCDR"),
     "Missed.Cleavage_mpwR" = c(0, 1, 1, 2, 0)
   )

   output <- generate_summary_Report(input_df = generic, analysis_name = "test", software = "Generic")

   expect_s3_class(output, "data.frame")
   expect_equal(nrow(output), 1)
   expect_equal(ncol(output), 18)
   expect_equal(output$Analysis, "test")
   expect_equal(output$`Median Peptide.IDs [abs.]`, 1)
   expect_equal(output$`Full profile - Precursor.IDs [abs.]`, 1)
   expect_equal(output$`Full profile - Precursor.IDs [%]`, 50)
   expect_equal(output$`Precursor.IDs [abs.] with a CV Retention time < 5 [%]`, 0)
   expect_equal(output$`Proteingroup.IDs [abs.] with a CV LFQ < 20 [%]`, 0)
   expect_equal(is.na(output$`Median Protein.IDs [abs.]`), FALSE)
   expect_equal(is.na(output$`Full profile - Protein.IDs [abs.]`), FALSE)
   expect_equal(is.na(output$`Full profile - Protein.IDs [%]`), FALSE)
   expect_equal(output$`Peptide IDs with zero missed cleavages [%]`, 40)

})

