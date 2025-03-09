
#get_DC_Report
test_that("get_DC_Report works", {

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

    output <- get_DC_Report(input_list = data, metric = "absolute")
    expect_type(output, "list")
    expect_equal(length(output), 5)
    expect_equal(names(output), c("A", "B", "C", "D", "Generic"))
    expect_equal(output[["A"]]$Protein.IDs, c(0, 5))

    output <- get_DC_Report(input_list = data, metric = "percentage")
    expect_type(output, "list")
    expect_equal(length(output), 5)
    expect_equal(names(output), c("A", "B", "C", "D", "Generic"))
    expect_equal(output[["A"]]$Protein.IDs, c(0, 100))

    #test error messages
    expect_error(get_DC_Report(input_list = data, metric = "absolte"), "Please check your metric entry - only use absolute or percentage")

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
                #    ProteinGroup.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4)
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
              #      Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 4),
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

    expect_error(get_DC_Report(input_list = data, metric = "absolute"))

})

#plot_DC_barplot
test_that("plot_DC_barplot works", {

 data <- list(
    "A" = tibble::tibble(
       Analysis = c("A", "A", "A"),
       Nr.Missing.Values = c(2, 1, 0),
       Precursor.IDs = c(50, 200, 4500),
       Peptide.IDs = c(30, 190, 3000),
       Protein.IDs = c(20, 40, 600),
       ProteinGroup.IDs = c(15, 30, 450),
       Profile = c("unique", "shared with at least 50%", "complete")
    ),
    "B" = tibble::tibble(
        Analysis = c("B", "B", "B"),
        Nr.Missing.Values = c(2, 1, 0),
        Precursor.IDs = c(50, 180, 4600),
        Peptide.IDs = c(50, 170, 3200),
        Protein.IDs = c(20, 40, 500),
        ProteinGroup.IDs = c(15, 30, 400),
        Profile = c("unique", "shared with at least 50%", "complete")
    )
 )


 output <- plot_DC_barplot(input_list = data, level = "Precursor.IDs", label = "absolute")

 expect_type(output, "list")
 expect_s3_class(output[["A"]], "ggplot")
 expect_s3_class(output[["B"]], "ggplot")

 output <- plot_DC_barplot(input_list = data, level = "Precursor.IDs", label = "percentage")

 expect_type(output, "list")
 expect_s3_class(output[["A"]], "ggplot")
 expect_s3_class(output[["B"]], "ggplot")

 output <- plot_DC_barplot(input_list = data, level = "Peptide.IDs", label = "absolute")

 expect_type(output, "list")
 expect_s3_class(output[["A"]], "ggplot")
 expect_s3_class(output[["B"]], "ggplot")

 output <- plot_DC_barplot(input_list = data, level = "Peptide.IDs", label = "percentage")

 expect_type(output, "list")
 expect_s3_class(output[["A"]], "ggplot")
 expect_s3_class(output[["B"]], "ggplot")

 output <- plot_DC_barplot(input_list = data, level = "Protein.IDs", label = "absolute")

 expect_type(output, "list")
 expect_s3_class(output[["A"]], "ggplot")
 expect_s3_class(output[["B"]], "ggplot")

 output <- plot_DC_barplot(input_list = data, level = "Protein.IDs", label = "percentage")

 expect_type(output, "list")
 expect_s3_class(output[["A"]], "ggplot")
 expect_s3_class(output[["B"]], "ggplot")

 output <- plot_DC_barplot(input_list = data, level = "ProteinGroup.IDs", label = "absolute")

 expect_type(output, "list")
 expect_s3_class(output[["A"]], "ggplot")
 expect_s3_class(output[["B"]], "ggplot")

 output <- plot_DC_barplot(input_list = data, level = "ProteinGroup.IDs", label = "percentage")

 expect_type(output, "list")
 expect_s3_class(output[["A"]], "ggplot")
 expect_s3_class(output[["B"]], "ggplot")

 #test error messages
 expect_error(plot_DC_barplot(input_list = data, level = "Preursor.IDs", label = "absolute"), "Please check your level entry - only use Precursor.IDs, Peptide.IDs, Protein.IDs or ProteinGroup.IDs")
 expect_error(plot_DC_barplot(input_list = data, level = "Precursor.IDs", label = "absolte"), "Please check your label entry - only use absolute or percentage")

 data <- list(
     "A" = tibble::tibble(
         Analysis = c("A", "A", "A"),
         Precursor.IDs = c(50, 200, 4500),
         Peptide.IDs = c(30, 190, 3000),
         Protein.IDs = c(20, 40, 600),
         ProteinGroup.IDs = c(15, 30, 450),
         Profile = c("unique", "shared with at least 50%", "complete")
     ),
     "B" = tibble::tibble(
         Analysis = c("B", "B", "B"),
         Nr.Missing.Values = c(2, 1, 0),
         Precursor.IDs = c(50, 180, 4600),
         Peptide.IDs = c(50, 170, 3200),
         Protein.IDs = c(20, 40, 500),
         ProteinGroup.IDs = c(15, 30, 400),
         Profile = c("unique", "shared with at least 50%", "complete")
     )
 )

 expect_error(plot_DC_barplot(input_list = data, level = "Precursor.IDs", label = "absolute"), "Wrong input detected - each input report requires the following columns Analysis, Nr.Missing.Values, ProteinGroup.IDs, Protein.IDs, Peptide.IDs, Precursor.IDs, Profile")

 })

#plot_DC_stacked_barplot
test_that("plot_DC_stacked_barplot works", {

    data <- list(
        "A" = tibble::tibble(
            Analysis = c("A", "A", "A"),
            Nr.Missing.Values = c(2, 1, 0),
            Precursor.IDs = c(50, 200, 4500),
            Peptide.IDs = c(30, 190, 3000),
            Protein.IDs = c(20, 40, 600),
            ProteinGroup.IDs = c(15, 30, 450),
            Profile = c("unique", "shared with at least 50%", "complete")
        ),
        "B" = tibble::tibble(
            Analysis = c("B", "B", "B"),
            Nr.Missing.Values = c(2, 1, 0),
            Precursor.IDs = c(50, 180, 4600),
            Peptide.IDs = c(50, 170, 3200),
            Protein.IDs = c(20, 40, 500),
            ProteinGroup.IDs = c(15, 30, 400),
            Profile = c("unique", "shared with at least 50%", "complete")
        )
    )

    output <- plot_DC_stacked_barplot(input_list = data, level = "Precursor.IDs", label = "absolute")
    expect_s3_class(output, "ggplot")

    output <- plot_DC_stacked_barplot(input_list = data, level = "Precursor.IDs", label = "percentage")
    expect_s3_class(output, "ggplot")

    output <- plot_DC_stacked_barplot(input_list = data, level = "Protein.IDs", label = "absolute")
    expect_s3_class(output, "ggplot")

    output <- plot_DC_stacked_barplot(input_list = data, level = "Protein.IDs", label = "percentage")
    expect_s3_class(output, "ggplot")

    output <- plot_DC_stacked_barplot(input_list = data, level = "Peptide.IDs", label = "absolute")
    expect_s3_class(output, "ggplot")

    output <- plot_DC_stacked_barplot(input_list = data, level = "Peptide.IDs", label = "percentage")
    expect_s3_class(output, "ggplot")

    output <- plot_DC_stacked_barplot(input_list = data, level = "ProteinGroup.IDs", label = "absolute")
    expect_s3_class(output, "ggplot")

    output <- plot_DC_stacked_barplot(input_list = data, level = "ProteinGroup.IDs", label = "percentage")
    expect_s3_class(output, "ggplot")


    #test error messages
    expect_error(plot_DC_stacked_barplot(input_list = data, level = "Preursor.IDs", label = "absolute"), "Please check your level entry - only use Precursor.IDs, Peptide.IDs, Protein.IDs or ProteinGroup.IDs")
    expect_error(plot_DC_stacked_barplot(input_list = data, level = "Precursor.IDs", label = "absolte"), "Please check your label entry - only use absolute or percentage")

    data <- list(
        "A" = tibble::tibble(
            Analysis = c("A", "A", "A"),
            Precursor.IDs = c(50, 200, 4500),
            Peptide.IDs = c(30, 190, 3000),
            Protein.IDs = c(20, 40, 600),
            ProteinGroup.IDs = c(15, 30, 450),
            Profile = c("unique", "shared with at least 50%", "complete")
        ),
        "B" = tibble::tibble(
            Analysis = c("B", "B", "B"),
            Nr.Missing.Values = c(2, 1, 0),
            Precursor.IDs = c(50, 180, 4600),
            Peptide.IDs = c(50, 170, 3200),
            Protein.IDs = c(20, 40, 500),
            ProteinGroup.IDs = c(15, 30, 400),
            Profile = c("unique", "shared with at least 50%", "complete")
        )
    )

    expect_error(plot_DC_stacked_barplot(input_list = data, level = "Precursor.IDs", label = "absolute"), "Wrong input detected - each input report requires the following columns Analysis, Nr.Missing.Values, ProteinGroup.IDs, Protein.IDs, Peptide.IDs, Precursor.IDs, Profile")

})
