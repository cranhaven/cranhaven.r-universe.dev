
#get_ID_Report
test_that("get_ID_Report works", {

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
                    Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 6),
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
                ),
                "pg" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 10),
                    ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
                )
        )
        ),
        Generic = list(
          filename = "Generic",
          software = "Generic",
          data = list(
            "Generic" = tibble::tibble(
              Run_mpwR = rep(c("A","B"), times = 15),
              Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 6),
              Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 6),
              Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 6),
              ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 6)
            )
          )
        )
    )

    output <- get_ID_Report(input_list = data)
    expect_type(output, "list")
    expect_equal(length(output), 5)
    expect_equal(names(output), c("A", "B", "C", "D", "Generic"))
    expect_equal(output[["A"]]$Protein.IDs, c(5, 5))

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
                    Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 6),
                    Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 6)
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
                ),
                "pg" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 10),
                    ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
                )
            )
        )
    )

    expect_error(get_ID_Report(input_list = data))

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
                    ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 6),
                    Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 6)
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
            #        Peptide.IDs_mpwR = rep(c("A", "A", "B", "B", "C"), each = 4),
                    ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
                ),
                "prot" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 10),
                    Protein.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
                ),
                "pg" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 10),
             #       ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
                )
            )
        )
    )

    expect_error(get_ID_Report(input_list = data))

})

#plot_ID_barplot
test_that("plot_ID_barplot works", {

 data <- list(
    "A" = tibble::tibble(
       Analysis = c("A", "A", "A"),
       Run = c("R01", "R02", "R03"),
       Precursor.IDs = c(4597, 4602, 4585),
       Peptide.IDs = c(3194, 3200, 3185),
       Protein.IDs = c(538, 542, 538),
       ProteinGroup.IDs = c(487, 490, 486)
    ),
    "B" = tibble::tibble(
        Analysis = c("B", "B", "B"),
        Run = c("R01", "R02", "R03"),
        Precursor.IDs = c(4597, 4602, 4585),
        Peptide.IDs = c(3194, 3200, 3185),
        Protein.IDs = c(538, 542, 538),
        ProteinGroup.IDs = c(487, 490, 486)
    )
 )

 output <- plot_ID_barplot(input_list = data, level = "Precursor.IDs")

 expect_type(output, "list")
 expect_s3_class(output[["A"]], "ggplot")
 expect_s3_class(output[["B"]], "ggplot")


 #test error messages
 expect_error(plot_ID_barplot(input_list = data, level = "Preursor.IDs"), "Please check your level entry - only use Precursor.IDs, Peptide.IDs, Protein.IDs or ProteinGroup.IDs")

 data <- list(
     "A" = tibble::tibble(
         Analysis = c("A", "A", "A"),
         Run = c("R01", "R02", "R03"),
         Peptide.IDs = c(3194, 3200, 3185),
         Protein.IDs = c(538, 542, 538),
         ProteinGroup.IDs = c(487, 490, 486)
     ),
     "B" = tibble::tibble(
         Analysis = c("B", "B", "B"),
         Run = c("R01", "R02", "R03"),
         Precursor.IDs = c(4597, 4602, 4585),
         Peptide.IDs = c(3194, 3200, 3185),
         Protein.IDs = c(538, 542, 538),
         ProteinGroup.IDs = c(487, 490, 486)
     )
 )

 #test error messages
 expect_error(plot_ID_barplot(input_list = data, level = "Precursor.IDs"), "Wrong input detected - each input report requires the following columns Analysis, Run, ProteinGroup.IDs, Protein.IDs, Peptide.IDs, Precursor.IDs")


})

#plot_ID_boxplot
test_that("plot_ID_boxplot works", {


    data <- list(
        "A" = tibble::tibble(
            Analysis = c("A", "A", "A"),
            Run = c("R01", "R02", "R03"),
            Precursor.IDs = c(4597, 4602, 4585),
            Peptide.IDs = c(3194, 3200, 3185),
            Protein.IDs = c(538, 542, 538),
            ProteinGroup.IDs = c(487, 490, 486)
        ),
        "B" = tibble::tibble(
            Analysis = c("B", "B", "B"),
            Run = c("R01", "R02", "R03"),
            Precursor.IDs = c(4597, 4602, 4585),
            Peptide.IDs = c(3194, 3200, 3185),
            Protein.IDs = c(NA, NA, NA),
            ProteinGroup.IDs = c(487, 490, 486)
        )
    )

    output <- plot_ID_boxplot(input_list = data, level = "Precursor.IDs")
    expect_s3_class(output, "ggplot")

    output <- plot_ID_boxplot(input_list = data, level = "Peptide.IDs")
    expect_s3_class(output, "ggplot")

    output <- plot_ID_boxplot(input_list = data, level = "Protein.IDs") #NAs are removed - B Analysis not shown
    expect_s3_class(output, "ggplot")

    output <- plot_ID_boxplot(input_list = data, level = "ProteinGroup.IDs")
    expect_s3_class(output, "ggplot")

    #test error messages
    expect_error(plot_ID_boxplot(input_list = data, level = "Preursor.IDs"), "Please check your level entry - only use Precursor.IDs, Peptide.IDs, Protein.IDs or ProteinGroup.IDs")

    data <- list(
        "A" = tibble::tibble(
            Analysis = c("A", "A", "A"),
            Run = c("R01", "R02", "R03"),
            Peptide.IDs = c(3194, 3200, 3185),
            Protein.IDs = c(538, 542, 538),
            ProteinGroup.IDs = c(487, 490, 486)
        ),
        "B" = tibble::tibble(
            Analysis = c("B", "B", "B"),
            Run = c("R01", "R02", "R03"),
            Precursor.IDs = c(4597, 4602, 4585),
            Peptide.IDs = c(3194, 3200, 3185),
            Protein.IDs = c(NA, NA, NA),
            ProteinGroup.IDs = c(487, 490, 486)
        )
    )

    #test error messages
    expect_error(plot_ID_boxplot(input_list = data, level = "Precursor.IDs"), "Wrong input detected - each input report requires the following columns Analysis, Run, ProteinGroup.IDs, Protein.IDs, Peptide.IDs, Precursor.IDs")

})
