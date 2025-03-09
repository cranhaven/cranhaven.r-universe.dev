
#get_CV_LFQ_pep
test_that("get_CV_LFQ_pep works", {

    set.seed(1234)
    data <- list(
        MQ = list(
            filename = "A",
            software = "MaxQuant",
            data = list(
                "ev" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    Retention.time_mpwR = rep(c(5, 5, 6, 6, 7), each = 2)
                    ),
                "pep" = tibble::tibble(
                    "Stripped.Sequence_mpwR" = c("A", "B", "C"),
                    "Intensity 01" = c(4, 4, 3.9),
                    "Intensity 02" = c(3, 3.5, 4),
                    "Intensity 03" = c(3, 3.5, 4),
                    "LFQ intensity 01" = c(4, 4, 0),
                    "LFQ intensity 02" = c(3, 3.5, NA),
                    "LFQ intensity 03" = c(3, 3.5, 4)
                    ),
                "pg" = tibble::tibble(
                    "ProteinGroup.IDs_mpwR" = c("A", "B", "C"),
                    "Intensity 01" = c(4, 4, 3.9),
                    "Intensity 02" = c(3, 3.5, 4),
                    "Intensity 03" = c(3, 3.5, 4),
                    "LFQ intensity 01" = c(4, 4, 0),
                    "LFQ intensity 02" = c(3, 3.5, NA),
                    "LFQ intensity 03" = c(3, 3.5, 4)
                )
            )
        ),
        DIANN = list(
            filename = "B",
            software = "DIA-NN",
            data = list(
                "DIA-NN" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    Retention.time_mpwR = sample(1:20, 10),
                    ProteinGroup_LFQ_mpwR = sample(1:30, 10))
            )
        ),
        Spectronaut = list(
            filename = "C",
            software = "Spectronaut",
            data = list(
                "Spectronaut" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    Peptide.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    Stripped.Sequence_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    Retention.time_mpwR = sample(1:20, 10),
                    Peptide_LFQ_mpwR = sample(1:30, 10),
                    ProteinGroup_LFQ_mpwR = sample(1:30, 10))
            )
        ),
        PD = list(
            filename = "D",
            software = "PD",
            data = list(
                "psm" = tibble::tibble(
                    Retention.time_mpwR = sample(1:20, 10),
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2)
            )
        )
        ),
        Generic = list(
          filename = "Generic",
          software = "Generic",
          data = list(
            "Generic" = tibble::tibble(
              Run_mpwR = rep(c("A","B"), times = 5),
              Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
              Peptide.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
              Stripped.Sequence_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
              ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
              Retention.time_mpwR = sample(1:20, 10),
              Peptide_LFQ_mpwR = sample(1:30, 10),
              ProteinGroup_LFQ_mpwR = sample(1:30, 10))
          )
        )
    )

    output <- get_CV_LFQ_pep(input_list = data)
    expect_type(output, "list")
    expect_equal(length(output), 3) #DIA-NN/PD removed
    expect_equal(names(output), c("A", "C", "Generic"))

    data <- list(
        MQ = list(
            filename = "A",
            software = "MaxQuant",
            data = list(
                "ev" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    Retention.time_mpwR = rep(c(5, 5, 6, 6, 7), each = 2)
                ),
                "pep" = tibble::tibble(
                    "Stripped.Sequence_mpwR" = c("A", "B", "C"),
                    "Intensity 01" = c(4, 4, 3.9),
                    "Intensity 02" = c(3, 3.5, 4),
                    "Intensity 03" = c(3, 3.5, 4),
                    "LFQ intensity 01" = c(4, 4, 0),
                    "LFQ intensity 02" = c(3, 3.5, NA),
                    "LFQ intensity 03" = c(3, 3.5, 4)
                ),
                "pg" = tibble::tibble(
                    "ProteinGroup.IDs_mpwR" = c("A", "B", "C"),
                    "Intensity 01" = c(4, 4, 3.9),
                    "Intensity 02" = c(3, 3.5, 4),
                    "Intensity 03" = c(3, 3.5, 4),
                    "LFQ intensity 01" = c(4, 4, 0),
                    "LFQ intensity 02" = c(3, 3.5, NA),
                    "LFQ intensity 03" = c(3, 3.5, 4)
                )
            )
        ),
        DIANN = list(
            filename = "B",
            software = "DIA-NN",
            data = list(
                "DIA-NN" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    Retention.time_mpwR = sample(1:20, 10),
                    ProteinGroup_LFQ_mpwR = sample(1:30, 10))
            )
        ),
        Spectronaut = list(
            filename = "C",
            software = "Spectronaut",
            data = list(
                "Spectronaut" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    Peptide.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    Retention.time_mpwR = sample(1:20, 10),
                    Peptide_LFQ_mpwR = sample(1:30, 10),
                    ProteinGroup_LFQ_mpwR = sample(1:30, 10))
            )
        ),
        PD = list(
            filename = "D",
            software = "PD",
            data = list(
                "psm" = tibble::tibble(
                    Retention.time_mpwR = sample(1:20, 10),
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2)
                )
            )
        )
    )

    expect_error(get_CV_LFQ_pep(input_list = data))

})

#get_CV_LFQ_pg
test_that("get_CV_LFQ_pg works", {

    set.seed(1234)
    data <- list(
        MQ = list(
            filename = "A",
            software = "MaxQuant",
            data = list(
                "ev" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    Retention.time_mpwR = rep(c(5, 5, 6, 6, 7), each = 2)
                ),
                "pep" = tibble::tibble(
                    "Stripped.Sequence_mpwR" = c("A", "B", "C"),
                    "Intensity 01" = c(4, 4, 3.9),
                    "Intensity 02" = c(3, 3.5, 4),
                    "Intensity 03" = c(3, 3.5, 4),
                    "LFQ intensity 01" = c(4, 4, 0),
                    "LFQ intensity 02" = c(3, 3.5, NA),
                    "LFQ intensity 03" = c(3, 3.5, 4)
                ),
                "pg" = tibble::tibble(
                    "ProteinGroup.IDs_mpwR" = c("A", "B", "C"),
                    "Intensity 01" = c(4, 4, 3.9),
                    "Intensity 02" = c(3, 3.5, 4),
                    "Intensity 03" = c(3, 3.5, 4),
                    "LFQ intensity 01" = c(4, 4, 0),
                    "LFQ intensity 02" = c(3, 3.5, NA),
                    "LFQ intensity 03" = c(3, 3.5, 4)
                )
            )
        ),
        DIANN = list(
            filename = "B",
            software = "DIA-NN",
            data = list(
                "DIA-NN" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    Retention.time_mpwR = sample(1:20, 10),
                    ProteinGroup_LFQ_mpwR = sample(1:30, 10))
            )
        ),
        Spectronaut = list(
            filename = "C",
            software = "Spectronaut",
            data = list(
                "Spectronaut" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    Peptide.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    Retention.time_mpwR = sample(1:20, 10),
                    Peptide_LFQ_mpwR = sample(1:30, 10),
                    ProteinGroup_LFQ_mpwR = sample(1:30, 10))
            )
        ),
        PD = list(
            filename = "D",
            software = "PD",
            data = list(
                "psm" = tibble::tibble(
                    Retention.time_mpwR = sample(1:20, 10),
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2)
                )
            )
        ),
        Generic = list(
          filename = "Generic",
          software = "Generic",
          data = list(
            "Generic" = tibble::tibble(
              Run_mpwR = rep(c("A","B"), times = 5),
              Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
              ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
              Retention.time_mpwR = sample(1:20, 10),
              ProteinGroup_LFQ_mpwR = sample(1:30, 10))
          )
        )
    )

    output <- get_CV_LFQ_pg(input_list = data)
    expect_type(output, "list")
    expect_equal(length(output), 4) #PD removed
    expect_equal(names(output), c("A", "B", "C","Generic"))

    data <- list(
        MQ = list(
            filename = "A",
            software = "MaxQuant",
            data = list(
                "ev" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    Retention.time_mpwR = rep(c(5, 5, 6, 6, 7), each = 2)
                ),
                "pep" = tibble::tibble(
                    "Stripped.Sequence_mpwR" = c("A", "B", "C"),
                    "Intensity 01" = c(4, 4, 3.9),
                    "Intensity 02" = c(3, 3.5, 4),
                    "Intensity 03" = c(3, 3.5, 4),
                    "LFQ intensity 01" = c(4, 4, 0),
                    "LFQ intensity 02" = c(3, 3.5, NA),
                    "LFQ intensity 03" = c(3, 3.5, 4)
                ),
                "pg" = tibble::tibble(
                    "ProteinGroup.IDs_mpwR" = c("A", "B", "C"),
                    "Intensity 01" = c(4, 4, 3.9),
                    "Intensity 02" = c(3, 3.5, 4),
                    "Intensity 03" = c(3, 3.5, 4),
                    "LFQ intensity 01" = c(4, 4, 0),
                    "LFQ intensity 02" = c(3, 3.5, NA),
                    "LFQ intensity 03" = c(3, 3.5, 4)
                )
            )
        ),
        DIANN = list(
            filename = "B",
            software = "DIA-NN",
            data = list(
                "DIA-NN" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    Retention.time_mpwR = sample(1:20, 10),
                    ProteinGroup_LFQ_mpwR = sample(1:30, 10))
            )
        ),
        Spectronaut = list(
            filename = "C",
            software = "Spectronaut",
            data = list(
                "Spectronaut" = tibble::tibble(
                 #   Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    Peptide.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    Retention.time_mpwR = sample(1:20, 10),
                    Peptide_LFQ_mpwR = sample(1:30, 10),
                    ProteinGroup_LFQ_mpwR = sample(1:30, 10))
            )
        ),
        PD = list(
            filename = "D",
            software = "PD",
            data = list(
                "psm" = tibble::tibble(
                    Retention.time_mpwR = sample(1:20, 10),
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2)
                )
            )
        )
    )

    expect_error(get_CV_LFQ_pg(input_list = data))

})

#get_CV_RT
test_that("get_CV_RT works", {

    set.seed(1234)
    data <- list(
        MQ = list(
            filename = "A",
            software = "MaxQuant",
            data = list(
                "ev" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    Retention.time_mpwR = rep(c(5, 5, 6, 6, 7), each = 2)
                ),
                "pep" = tibble::tibble(
                    "Stripped.Sequence_mpwR" = c("A", "B", "C"),
                    "Intensity 01" = c(4, 4, 3.9),
                    "Intensity 02" = c(3, 3.5, 4),
                    "Intensity 03" = c(3, 3.5, 4),
                    "LFQ intensity 01" = c(4, 4, 0),
                    "LFQ intensity 02" = c(3, 3.5, NA),
                    "LFQ intensity 03" = c(3, 3.5, 4)
                ),
                "pg" = tibble::tibble(
                    "ProteinGroup.IDs_mpwR" = c("A", "B", "C"),
                    "Intensity 01" = c(4, 4, 3.9),
                    "Intensity 02" = c(3, 3.5, 4),
                    "Intensity 03" = c(3, 3.5, 4),
                    "LFQ intensity 01" = c(4, 4, 0),
                    "LFQ intensity 02" = c(3, 3.5, NA),
                    "LFQ intensity 03" = c(3, 3.5, 4)
                )
            )
        ),
        DIANN = list(
            filename = "B",
            software = "DIA-NN",
            data = list(
                "DIA-NN" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    Retention.time_mpwR = sample(1:20, 10),
                    ProteinGroup_LFQ_mpwR = sample(1:30, 10))
            )
        ),
        Spectronaut = list(
            filename = "C",
            software = "Spectronaut",
            data = list(
                "Spectronaut" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    Peptide.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    Retention.time_mpwR = sample(1:20, 10),
                    Peptide_LFQ_mpwR = sample(1:30, 10),
                    ProteinGroup_LFQ_mpwR = sample(1:30, 10))
            )
        ),
        PD = list(
            filename = "D",
            software = "PD",
            data = list(
                "psm" = tibble::tibble(
                    Retention.time_mpwR = sample(1:20, 10),
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2)
                )
            )
        ),
        Generic = list(
          filename = "Generic",
          software = "Generic",
          data = list(
            "Generic" = tibble::tibble(
              Run_mpwR = rep(c("A","B"), times = 5),
              Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
              Peptide.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
              Stripped.Sequence_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
              ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
              Retention.time_mpwR = sample(1:20, 10),
              Peptide_LFQ_mpwR = sample(1:30, 10),
              ProteinGroup_LFQ_mpwR = sample(1:30, 10))
          )
        )
    )

    output <- get_CV_RT(input_list = data)
    expect_type(output, "list")
    expect_equal(length(output), 5)
    expect_equal(names(output), c("A", "B", "C", "D", "Generic"))

    data <- list(
        MQ = list(
            filename = "A",
            software = "MaxQuant",
            data = list(
                "ev" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    Retention.time_mpwR = rep(c(5, 5, 6, 6, 7), each = 2)
                ),
                "pep" = tibble::tibble(
                    "Stripped.Sequence_mpwR" = c("A", "B", "C"),
                    "Intensity 01" = c(4, 4, 3.9),
                    "Intensity 02" = c(3, 3.5, 4),
                    "Intensity 03" = c(3, 3.5, 4),
                    "LFQ intensity 01" = c(4, 4, 0),
                    "LFQ intensity 02" = c(3, 3.5, NA),
                    "LFQ intensity 03" = c(3, 3.5, 4)
                ),
                "pg" = tibble::tibble(
                    "ProteinGroup.IDs_mpwR" = c("A", "B", "C"),
                    "Intensity 01" = c(4, 4, 3.9),
                    "Intensity 02" = c(3, 3.5, 4),
                    "Intensity 03" = c(3, 3.5, 4),
                    "LFQ intensity 01" = c(4, 4, 0),
                    "LFQ intensity 02" = c(3, 3.5, NA),
                    "LFQ intensity 03" = c(3, 3.5, 4)
                )
            )
        ),
        DIANN = list(
            filename = "B",
            software = "DIA-NN",
            data = list(
                "DIA-NN" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    Retention.time_mpwR = sample(1:20, 10),
                    ProteinGroup_LFQ_mpwR = sample(1:30, 10))
            )
        ),
        Spectronaut = list(
            filename = "C",
            software = "Spectronaut",
            data = list(
                "Spectronaut" = tibble::tibble(
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2),
                    Peptide.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                    ProteinGroup.IDs_mpwR = rep(c("A", "B", "C", "D", "E"), each = 2),
                  #  Retention.time_mpwR = sample(1:20, 10),
                    Peptide_LFQ_mpwR = sample(1:30, 10),
                    ProteinGroup_LFQ_mpwR = sample(1:30, 10))
            )
        ),
        PD = list(
            filename = "D",
            software = "PD",
            data = list(
                "psm" = tibble::tibble(
                    Retention.time_mpwR = sample(1:20, 10),
                    Run_mpwR = rep(c("A","B"), times = 5),
                    Precursor.IDs_mpwR = rep(c("A2", "A3", "B2", "B3", "C1"), each = 2)
                )
            )
        )
    )

    expect_error(get_CV_RT(input_list = data))

})

#plot_CV_density
test_that("plot_CV_density works", {

 set.seed(123)
 data <- list(
    "A" = tibble::tibble(
       Analysis_mpwR = rep("A", times = 10),
       CV_Retention.time_mpwR = sample(1:20, 10),
       CV_Peptide_LFQ_mpwR = sample(1:30, 10),
       CV_ProteinGroup_LFQ_mpwR = sample(1:30, 10)),
    "B" = tibble::tibble(
       Analysis_mpwR = rep("B", times = 10),
       CV_Retention.time_mpwR = sample(1:20, 10),
       CV_Peptide_LFQ_mpwR = sample(1:30, 10),
       CV_ProteinGroup_LFQ_mpwR = sample(1:30, 10))
    )

 output <- plot_CV_density(input_list = data, cv_col = "RT")
 expect_s3_class(output, "ggplot")
 expect_equal(output[["labels"]][["x"]], "\nRetention time CV [%] at precursor level")

 output <- plot_CV_density(input_list = data, cv_col = "Pep_quant")
 expect_s3_class(output, "ggplot")
 expect_equal(output[["labels"]][["x"]], "\nLFQ CV [%] at peptide level")

 output <- plot_CV_density(input_list = data, cv_col = "PG_quant")
 expect_s3_class(output, "ggplot")
 expect_equal(output[["labels"]][["x"]], "\nLFQ CV [%] at proteingroup level")

 #test error message
 expect_error(plot_CV_density(input_list = data, cv_col = "RTs"), "Please check your cv_col entry - only use RT, Pep_quant or PG_quant")

 })
