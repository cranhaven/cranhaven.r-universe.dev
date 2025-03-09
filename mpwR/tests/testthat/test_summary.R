
#get_summary_Report
test_that("get_summary_Report works",{

   data <- list(
      MQ = list(
         filename = "A",
         software = "MaxQuant",
         data = list(
            "ev" = tibble::tibble(
               "Run_mpwR" = c("R01", "R01", "R02", "R03", "R01"),
               "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "A1", "B2"), #same precursor multiple times per run possible - first RT entry used
               "Retention.time_mpwR" = c(3, 3.5, 4, 5, 4),
               "Peptide.IDs_mpwR" = c("A", "A", "A", "A", "B"),
               "Protein.IDs_mpwR" = c("A", "A", "A", "A", "B")
            ),
            "pep" = tibble::tibble(
               "Stripped.Sequence_mpwR" = c("A", "B", "C"),
               "Missed.Cleavage_mpwR" = c(0, 1, 2),
               "Intensity 01" = c(4, 4, 3.9),
               "Intensity 02" = c(3, 3.5, 4),
               "Intensity 03" = c(3, 3.5, 4),
               "LFQ Intensity 01" = c(4, 4, 3.9),
               "LFQ Intensity 02" = c(3, 3.5, 4),
               "LFQ Intensity 03" = c(3, 3.5, 4)
            ),
            "pg" = tibble::tibble(
               "ProteinGroup.IDs_mpwR" = c("A", "B", "C"),
               "Intensity 01" = c(4, 4, 3.9),
               "Intensity 02" = c(3, 3.5, 4),
               "Intensity 03" = c(3, 3.5, 4),
               "LFQ Intensity 01" = c(4, 4, 3.9),
               "LFQ Intensity 02" = c(3, 3.5, 4),
               "LFQ Intensity 03" = c(3, 3.5, 4)
            )
         )
      ),
      DIANN = list(
         filename = "B",
         software = "DIA-NN",
         data = list(
            "DIA-NN" = tibble::tibble(
               "Run_mpwR" = c("R01", "R01", "R02", "R03", "R01"),
               "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "A1", "B2"), #same precursor multiple times per run possible - first RT entry used
               "Retention.time_mpwR" = c(3, 3.5, 4, 5, 4),
               "ProteinGroup_LFQ_mpwR" = c(3, 4, 5, 4, 4),
               "Peptide.IDs_mpwR" = c("A", "A", "A", "A", "B"),
               "Protein.IDs_mpwR" = c("A", "A", "A", "A", "B"),
               "ProteinGroup.IDs_mpwR" = c("A", "A", "A", "A", "B"),
               "Stripped.Sequence_mpwR" = c("ABCR", "AKCR", "ABKCK", "ARKAR", "ABCDR")
            )
         )
      ),
      Spectronaut = list(
         filename = "C",
         software = "Spectronaut",
         data = list(
            "Spectronaut" = tibble::tibble(
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
         )
      ),
      PD = list(
         filename = "D",
         software = "PD",
         data = list(
            "psm" = tibble::tibble(
               "Run_mpwR" = c("R01", "R01", "R02", "R03", "R01"),
               "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "A1", "B2"), #same precursor multiple times per run possible - first RT entry used
               "Retention.time_mpwR" = c(3, 3.5, 4, 5, 4),
               "Peptide.IDs_mpwR" = c("A", "A", "A", "A", "B"),
               "Protein.IDs_mpwR" = c("A", "A", "A", "A", "B")
            ),
            "pep" = tibble::tibble(
               "Stripped.Sequence_mpwR" = c("A", "B", "C"),
               "Missed.Cleavage_mpwR" = c(0, 1, 2)
            ),
            "prot" = tibble::tibble(
               "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
               "Protein.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C")
            ),
            "pg" = tibble::tibble(
               "Run_mpwR" = c("R01", "R02", "R03", "R01", "R02", "R03", "R02", "R03"),
               "ProteinGroup.IDs_mpwR" = c("A", "A", "A", "B", "B", "B", "C", "C") #only number in original data frame
            )
         )
      ),
      Generic = list(
        filename = "Generic",
        software = "Generic",
        data = list(
          "Generic" = tibble::tibble(
            "Run_mpwR" = c("R01", "R01", "R02", "R03", "R01"),
            "Precursor.IDs_mpwR" = c("A1", "A1", "A1", "A1", "B2"), #same precursor multiple times per run possible - first RT entry used
            "Retention.time_mpwR" = c(3, 3.5, 4, 5, 4),
            "ProteinGroup_LFQ_mpwR" = c(3, 4, 5, 4, 4),
            "Peptide_LFQ_mpwR" = c(3, 4, 5, 4, NA),
            "Peptide.IDs_mpwR" = c("A", "A", "A", "A", "B"),
            "ProteinGroup.IDs_mpwR" = c("A", "A", "A", "A", "B"),
            "Protein.IDs_mpwR" = c("A", "A", "A", "A", "B"),
            "Stripped.Sequence_mpwR" = c("ABCR", "AKCR", "ABKCK", "ARKAR", "ABCDR"),
            "Missed.Cleavage_mpwR" = c(0, 1, 1, 2, 0),
          )
        )
      )
   )

 output <- get_summary_Report(input_list = data)

 expect_s3_class(output, "data.frame")
 expect_equal(nrow(output), length(data))
 expect_equal(ncol(output), 18)
 expect_equal(output$`Peptide.IDs [abs.] with a CV LFQ < 20 [%]`, c(3, NA, 0, NA, 0))

})

