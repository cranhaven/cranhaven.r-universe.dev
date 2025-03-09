
#get_Upset_list
test_that("get_Upset_list works", {

   data <- list(
      MQ = list(
         filename = "A",
         software = "MaxQuant",
         data = list(
            "ev" = tibble::tibble(
               Run_mpwR = rep(c("A","B"), times = 5),
               Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
               Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
               Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 2)
            ),
            "pep" = tibble::tibble(
               "Stripped.Sequence_mpwR" = c("A", "B", "C", "D", "E"),
               "Intensity 01" = c(4, 4, 3.9, 5, 5),
               "Intensity 02" = c(4, 4, 3.9, 5, 5)
            ),
            "pg" = tibble::tibble(
               "ProteinGroup.IDs_mpwR" = c("A", "B", "C", "D", "E"),
               "Intensity 01" = c(4, 4, 3.9, 5, 5),
               "Intensity 02" = c(4, 4, 3.9, 5, 5)
            )
         )
      ),
      DIANN = list(
         filename = "B",
         software = "DIA-NN",
         data = list(
            "DIA-NN" = tibble::tibble(
               Run_mpwR = rep(c("A","B"), times = 10),
               Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
               Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
               Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 4),
               ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
            )
         )
      ),
      Spectronaut = list(
         filename = "C",
         software = "Spectronaut",
         data = list(
            "Spectronaut" = tibble::tibble(
               Run_mpwR = rep(c("A","B"), times = 15),
               Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 6),
               Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 6),
               ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 6)
            )
         )
      ),
      PD = list(
         filename = "D",
         software = "PD",
         data = list(
            "psm" = tibble::tibble(
               Run_mpwR = rep(c("A","B"), times = 10),
               Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
               Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
               Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 4),
               ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
            ),
            "prot" = tibble::tibble(
               Run_mpwR = rep(c("A","B"), times = 10),
               Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
            )
         )
      ),
      Generic = list(
        filename = "Generic",
        software = "Generic",
        data = list(
          "Generic" = tibble::tibble(
            Run_mpwR = rep(c("A","B"), times = 10),
            Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
            Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
            Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 4),
            ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
          )
        )
      )
   )

   output <- get_Upset_list(input_list = data, level = "Precursor.IDs")

   expect_type(output, "list")
   expect_equal(length(output), 5)
   expect_equal(names(output), c("A", "B", "C", "D", "Generic"))
   expect_equal(output$A, c("A2", "A3", "B2", "B3", "C1"))

   output <- get_Upset_list(input_list = data, level = "Peptide.IDs")

   expect_type(output, "list")
   expect_equal(length(output), 5)
   expect_equal(names(output), c("A", "B", "C", "D", "Generic"))
   expect_equal(output$A, c("A", "B", "C"))

   output <- get_Upset_list(input_list = data, level = "Protein.IDs")

   expect_type(output, "list")
   expect_equal(length(output), 4) #Spectronaut removed
   expect_equal(names(output), c("A", "B", "D", "Generic")) #Spectronaut removed
   expect_equal(output$A, c("A2", "A3", "B2", "B3", "C1"))

   output <- get_Upset_list(input_list = data, level = "ProteinGroup.IDs")

   expect_type(output, "list")
   expect_equal(length(output), 5)
   expect_equal(names(output), c("A", "B", "C", "D", "Generic"))
   expect_equal(output$A, c("A", "B", "C", "D", "E"))

   data <- list(
      PD = list(
         filename = "D",
         software = "PD",
         data = list(
            "psm" = tibble::tibble(
               Run_mpwR = rep(c("A","B"), times = 10),
               Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
               Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
            ),
            "prot" = tibble::tibble(
               Run_mpwR = rep(c("A","B"), times = 10),
               Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
            )
         )
      )
   )

   expect_error(get_Upset_list(input_list = data, level = "Precursor.IDs"))

   data <- list(
      MQ = list(
         filename = "A",
         software = "MaxQuant",
         data = list(
            "ev" = tibble::tibble(
               Run_mpwR = rep(c("A","B"), times = 5),
               Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
               Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
               Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 2)
            ),
            "pep" = tibble::tibble(
               "Stripped.Sequence_mpwR" = c("A", "B", "C", "D", "E"),
               "Intensity 01" = c(4, 4, 3.9, 5, 5),
               "Intensity 02" = c(4, 4, 3.9, 5, 5)
            ),
            "pg" = tibble::tibble(
               "ProteinGroup.IDs_mpwR" = c("A", "B", "C", "D", "E"),
               "Intensity 01" = c(4, 4, 3.9, 5, 5),
               "Intensity 02" = c(4, 4, 3.9, 5, 5)
            )
         )
      ),
      DIANN = list(
         filename = "B",
         software = "DIA-NN",
         data = list(
            "DIA-NN" = tibble::tibble(
               Run_mpwR = rep(c("A","B"), times = 10),
               Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
               Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
               Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 4),
               ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
            )
         )
      ),
      Spectronaut = list(
         filename = "C",
         software = "Spectronaut",
         data = list(
            "Spectronaut" = tibble::tibble(
               Run_mpwR = rep(c("A","B"), times = 15),
               Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 6),
               Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 6),
               ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 6)
            )
         )
      ),
      PD = list(
         filename = "D",
         software = "PD",
         data = list(
            "psm" = tibble::tibble(
               Run_mpwR = rep(c("A","B"), times = 10),
               Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
               Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
               Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 4),
               ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
            ),
            "prot" = tibble::tibble(
               Run_mpwR = rep(c("A","B"), times = 10),
             #  Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
            )
         )
      )
   )

   expect_error(get_Upset_list(input_list = data))

})

#plot_Upset
test_that("plot_Upset works", {

   data <- list(
      "A" = c("A", "B", "C", "D"),
      "B" = c("A", "B", "C", "F"),
      "C" = c("A", "B", "G", "E")
   )

   output <- plot_Upset(input_list = data, label = "Precursor.IDs")
   expect_s3_class(output, "upset")

   output <- plot_Upset(input_list = data, label = "Precursor.IDs", highlight_overlap = TRUE)
   expect_s3_class(output, "upset")
})
