context("test_utlis")

#analyze_unkown_mods
test_that("analyze_unknown_mods is succesful", {

    data <- tibble::tibble(
     "traceR_mod.peptides" = c("AACLLPK", "ALTDM(UniMod:35)PQM(UniMod:35)R", "ALTDM(DummyModification)PQMK", "ALTDM(UniMod:35)PQM(UniMod:35)R", "ALTDM(DummyModification)PQMK"),
     "traceR_mod.peptides_unknownMods" = c(FALSE, FALSE, TRUE, FALSE, TRUE),
     "traceR_precursor" = c("AACLLPK2", "ALTDM(UniMod:35)PQM(UniMod:35)R2", "ALTDM(DummyModification)PQMK3", "ALTDM(UniMod:35)PQM(UniMod:35)R2", "ALTDM(DummyModification)PQMK3"),
     "traceR_precursor_unknownMods" = c(FALSE, FALSE, TRUE, FALSE, TRUE)
    )

  #precursor level
    output_plot <- analyze_unknown_mods(input_df = data, level = "precursor", plot = TRUE, plot_characteristic = "relative")
    output_report <- analyze_unknown_mods(input_df = data, level = "precursor", plot = FALSE)

    expect_error(analyze_unknown_mods(input_df = data[, 1:2], level = "precursor", plot = FALSE), "For precursor level: traceR_precursor and traceR_precursor_unknownMods columns must be present in submitted data.")
    expect_s3_class(output_plot, "ggplot")
    expect_s3_class(output_report, "tbl")
    expect_equal(nrow(output_report), 2)
    expect_equal(ncol(output_report), 3)
    expect_equal(output_report$absolute_count , c(2, 1))

  #modified peptide level
    output_plot <- analyze_unknown_mods(input_df = data, level = "modified_peptides", plot = TRUE, plot_characteristic = "relative")
    output_report <- analyze_unknown_mods(input_df = data, level = "modified_peptides", plot = FALSE)

    expect_error(analyze_unknown_mods(input_df = data[, 3:4], level = "modified_peptides", plot = FALSE), "For peptide level: traceR_mod.peptides and traceR_mod.peptides_unknownMods columns must be present in submitted data.")
    expect_s3_class(output_plot, "ggplot")
    expect_s3_class(output_report, "tbl")
    expect_equal(nrow(output_report), 2)
    expect_equal(ncol(output_report), 3)
    expect_equal(output_report$absolute_count , c(2, 1))

})

#sort_string_pg
test_that("sort_string_pg is succesful", {

  data <- tibble::tibble(
    "ProteinGroups" = c("P0234;   A03235", "XCasdk1; B0234s")
  )

  output <- sort_string_pg(input_df = data, sort_column = "ProteinGroups", split_pattern = ";")

  expect_type(output, "character")
  expect_equal(length(output), 2)
  expect_equal(output , c("A03235;P0234", "B0234s;XCasdk1"))

})

#analyze_string_pg
test_that("analyze_string_pg is succesful", {

  data <- tibble::tibble(
    "ProteinGroups_input_df1" = c("A03235;P0234", "B0234s", "Q65089"),
    "traceR_precursor" = c("A1", "B2", "C2"),
    "ProteinGroups_input_df2" = c("P0234", "A0505YV", "Q65089")
  )

  output <- analyze_string_pg(input_df = data)

  expect_s3_class(output, "tbl")
  expect_equal(nrow(output), 1)
  expect_equal(output$ProteinGroups_input_df2 , c("A0505YV"))

})
