
#get_MC_Report
test_that("get_MC_Report works", {

    data <- list(
        MQ = list(
            filename = "A",
            software = "MaxQuant",
            data = list(
                "pep" = tibble::tibble(
                    Stripped.Sequence_mpwR = c("A", "B", "C", "D", "E"),
                    Missed.Cleavage_mpwR = c(0, 1, 1, 2, 2),
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
                    Stripped.Sequence_mpwR = c("ABCR", "AKCR", "ABKCK", "ARKAR", "ABCR") #0, 1, 1, 2, removed
                )
            )
        ),
        Spectronaut = list(
            filename = "C",
            software = "Spectronaut",
            data = list(
                "Spectronaut" = tibble::tibble(
                    Stripped.Sequence_mpwR = c("A", "B", "C", "D", "E"),
                    Missed.Cleavage_mpwR = c(0, 1, 1, 2, 2)
                )
            )
        ),
        PD = list(
            filename = "D",
            software = "PD",
            data = list(
                "pep" = tibble::tibble(
                    Stripped.Sequence_mpwR = c("A", "B", "C", "D", "E"),
                    Missed.Cleavage_mpwR = c(0, 1, 1, 2, 2)
                 )
        )
        ),
        Generic = list(
          filename = "Generic",
          software = "Generic",
          data = list(
            "Generic" = tibble::tibble(
              Stripped.Sequence_mpwR = c("A", "B", "C", "D", "E"),
              Missed.Cleavage_mpwR = c(0, 1, 1, 2, 2)
            )
          )
        )
    )

    output <- get_MC_Report(input_list = data, metric = "absolute")
    expect_type(output, "list")
    expect_equal(length(output), 5)
    expect_equal(names(output), c("A", "B", "C", "D", "Generic"))

    output <- get_MC_Report(input_list = data, metric = "percentage")
    expect_type(output, "list")
    expect_equal(length(output), 5)
    expect_equal(names(output), c("A", "B", "C", "D", "Generic"))

    #test error messages
    expect_error(get_MC_Report(input_list = data, metric = "absolte"), "Please check your metric entry - only use absolute or percentage")

    data <- list(
        PD = list(
            filename = "D",
            software = "PD",
            data = list(
                "pep" = tibble::tibble(
                    Stripped.Sequence_mpwR = c("A", "B", "C", "D", "E"),
                )
            )
        )
    )

    expect_error(get_MC_Report(input_list = data, metric = "absolute"))

    data <- list(
        MQ = list(
            filename = "A",
            software = "MaxQuant",
            data = list(
                "pep" = tibble::tibble(
                    Stripped.Sequence_mpwR = c("A", "B", "C", "D", "E"),
                    Missed.Cleavage_mpwR = c(0, 1, 1, 2, 2),
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
                    Stripped.Sequence_mpwR = c("ABCR", "AKCR", "ABKCK", "ARKAR", "ABCR") #0, 1, 1, 2, removed
                )
            )
        ),
        Spectronaut = list(
            filename = "C",
            software = "Spectronaut",
            data = list(
                "Spectronaut" = tibble::tibble(
                    Stripped.Sequence_mpwR = c("A", "B", "C", "D", "E"),
                    Missed.Cleavage_mpwR = c(0, 1, 1, 2, 2)
                )
            )
        ),
        PD = list(
            filename = "D",
            software = "PD",
            data = list(
                "pep" = tibble::tibble(
                  #  Stripped.Sequence_mpwR = c("A", "B", "C", "D", "E"),
                    Missed.Cleavage_mpwR = c(0, 1, 1, 2, 2)
                )
            )
        )
    )

    expect_error(get_MC_Report(input_list = data))
})

#plot_MC_barplot
test_that("plot_MC_barplot works", {

 data <- list(
    "A" = tibble::tibble(
        Analysis = c("A", "A", "A", "A", "A"),
        Missed.Cleavage = c("0", "1", "2", "3", "No R/K cleavage site"),
        mc_count = c("2513", "368", "23", "38", "10")
    ),
    "B" = tibble::tibble(
        Analysis = c("B", "B", "B", "B", "B"),
        Missed.Cleavage = c("0", "1", "2", "3", "No R/K cleavage site"),
        mc_count = c("2513", "368", "23", "38", "10")
    )
 )

 output <- plot_MC_barplot(input_list = data, label = "absolute")

 expect_type(output, "list")
 expect_s3_class(output[["A"]], "ggplot")
 expect_s3_class(output[["B"]], "ggplot")

 output <- plot_MC_barplot(input_list = data, label = "percentage")

 expect_type(output, "list")
 expect_s3_class(output[["A"]], "ggplot")
 expect_s3_class(output[["B"]], "ggplot")

 #test error messages
 expect_error(plot_MC_barplot(input_list = data, label = "absolte"), "Please check your label entry - only use absolute or percentage")

 data <- list(
     "A" = tibble::tibble(
         Analysis = c("A", "A", "A", "A", "A"),
         Missed.Cleavage = c("0", "1", "2", "3", "No R/K cleavage site")
     ),
     "B" = tibble::tibble(
         Analysis = c("B", "B", "B", "B", "B"),
         Missed.Cleavage = c("0", "1", "2", "3", "No R/K cleavage site"),
         mc_count = c("2513", "368", "23", "38", "10")
     )
 )

 expect_error(plot_MC_barplot(input_list = data, label = "absolute"), "Wrong input detected - each input report requires the following columns Analysis, Missed.Cleavage, mc_count")

})

#plot_MC_stacked_barplot
test_that("plot_MC_stacked_barplot works", {

    data <- list(
        "A" = tibble::tibble(
            Analysis = c("A", "A", "A", "A", "A"),
            Missed.Cleavage = c("0", "1", "2", "3", "No R/K cleavage site"),
            mc_count = c("2513", "368", "23", "38", "10")
        ),
        "B" = tibble::tibble(
            Analysis = c("B", "B", "B", "B", "B"),
            Missed.Cleavage = c("0", "1", "2", "3", "No R/K cleavage site"),
            mc_count = c("2513", "368", "23", "38", "10")
        )
    )

    output <- plot_MC_stacked_barplot(input_list = data, label = "absolute")
    expect_s3_class(output, "ggplot")

    output <- plot_MC_stacked_barplot(input_list = data, label = "percentage")
    expect_s3_class(output, "ggplot")

    #test error messages
    expect_error(plot_MC_stacked_barplot(input_list = data, label = "absolte"), "Please check your label entry - only use absolute or percentage")

    data <- list(
        "A" = tibble::tibble(
            Analysis = c("A", "A", "A", "A", "A"),
            Missed.Cleavage = c("0", "1", "2", "3", "No R/K cleavage site"),
            mc_count = c("2513", "368", "23", "38", "10")
        ),
        "B" = tibble::tibble(
            Analysis = c("B", "B", "B", "B", "B"),
            Missed.Cleavage = c("0", "1", "2", "3", "No R/K cleavage site")
        )
    )

    expect_error(plot_MC_stacked_barplot(input_list = data, label = "absolute"), "Wrong input detected - each input report requires the following columns Analysis, Missed.Cleavage, mc_count")

})
