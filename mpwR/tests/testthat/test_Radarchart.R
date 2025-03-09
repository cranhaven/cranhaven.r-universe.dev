
#plot_radarchart
test_that("plot_radarchart works", {

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

  output <- plot_radarchart(input_df = data)

  expect_s3_class(output, "plotly")

  data <- tibble::tibble(
    Analysis = c("A", "B")
  )
  expect_error(plot_radarchart(input_df = data), "Please add at least one category column.")

  data <- tibble::tibble(
    `Median ProteinGroup.IDs [abs.]` = c(5, 10)
  )
  expect_error(plot_radarchart(input_df = data), "No Analysis column detected.")

})
