context("test_analyses")

#analysis of unknown modifications
test_that("analyze_unknown_mods is succesful", {

   data <- tibble::tibble(
    "traceR_mod.peptides" = c("AACLLPK", "ALTDM(UniMod:35)PQM(UniMod:35)R", "ALTDM(DummyModification)PQMK", "ALTDM(UniMod:35)PQM(UniMod:35)R", "ALTDM(DummyModification)PQMK"),
    "traceR_mod.peptides_unknownMods" = c(FALSE, FALSE, TRUE, FALSE, TRUE),
    "traceR_precursor" = c("AACLLPK2", "ALTDM(UniMod:35)PQM(UniMod:35)R2", "ALTDM(DummyModification)PQMK3", "ALTDM(UniMod:35)PQM(UniMod:35)R2", "ALTDM(DummyModification)PQMK3"),
    "traceR_precursor_unknownMods" = c(FALSE, FALSE, TRUE, FALSE, TRUE)
   )

  #precursor level
  output_plot <- analyze_unknown_mods(input_df = data, level = "precursor", plot = TRUE, plot_characteristic = "absolute")
  output_report <- analyze_unknown_mods(input_df = data, level = "precursor", plot = FALSE)

  expect_error(analyze_unknown_mods(input_df = data[,1:2], level = "precursor", plot = TRUE, plot_characteristic = "absolute"), "For precursor level: traceR_precursor and traceR_precursor_unknownMods columns must be present in submitted data.")
  expect_s3_class(output_plot, "ggplot")
  expect_s3_class(output_report, "tbl")
  expect_equal(nrow(output_report), 2)
  expect_equal(ncol(output_report), 3)
  expect_equal(output_report$absolute_count , c(2, 1))

  #peptide level
  output_plot <- analyze_unknown_mods(input_df = data, level = "modified_peptides", plot = TRUE, plot_characteristic = "relative")
  output_report <- analyze_unknown_mods(input_df = data, level = "modified_peptides", plot = FALSE)

  expect_error(analyze_unknown_mods(input_df = data[,3:4], level = "modified_peptides", plot = TRUE, plot_characteristic = "absolute"), "For peptide level: traceR_mod.peptides and traceR_mod.peptides_unknownMods columns must be present in submitted data.")
  expect_s3_class(output_plot, "ggplot")
  expect_s3_class(output_report, "tbl")
  expect_equal(nrow(output_report), 2)
  expect_equal(ncol(output_report), 3)
  expect_equal(output_report$absolute_count , c(2, 1))

})

#analysis of unknown modifications
test_that("analyze_unknown_mods is succesful", {

   data <- tibble::tibble(
     "traceR_connected_pg_prec" = c("common_common", "common_unique", "unique_common"),
     "traceR_connected_mod.pep_prec" = c("common_common", "unique_unique", "common_common"),
     "traceR_traced_proteinGroups" = c("common", "common", "unique"),
     "traceR_traced_mod.peptides" = c("common", "unique", "common"),
     "traceR_traced_precursor" = c("common", "unique", "common"),
     "traceR_proteinGroups" = c("P02768", "P02671", "Q92496"),
     "traceR_mod.peptides" = c("AAC(UniMod:4)LLPK", "RLEVDIDIK", "EGIVEYPR"),
     "traceR_precursor" = c("AAC(UniMod:4)LLPK1", "RLEVDIDIK2", "EGIVEYPR2")
   )

   # proteingroup_precursor connection
   #upper
   upper_plot <- analyze_connected_levels(input_df = data, connected_levels = "proteinGroup_precursor",count_level = "upper", plot = TRUE, plot_characteristic = "relative")
   upper_report <- analyze_connected_levels(input_df = data, connected_levels = "proteinGroup_precursor",count_level = "upper", plot = FALSE)
   #lower
   lower_plot <- analyze_connected_levels(input_df = data, connected_levels = "proteinGroup_precursor",count_level = "lower", plot = TRUE, plot_characteristic = "relative")
   lower_report <- analyze_connected_levels(input_df = data, connected_levels = "proteinGroup_precursor",count_level = "lower", plot = FALSE)

   expect_error(analyze_connected_levels(input_df = data[,3:4], connected_levels = "proteinGroup_precursor",count_level = "upper", plot = TRUE, plot_characteristic = "relative"), "For connected levels - proteinGroup_precursor: traceR_connected_pg_prec column must be present in submitted data.")
   expect_s3_class(upper_plot, "ggplot")
   expect_s3_class(lower_plot, "ggplot")
   expect_s3_class(upper_report, "tbl")
   expect_s3_class(lower_report, "tbl")
   expect_equal(nrow(upper_report), 3)
   expect_equal(ncol(upper_report), 3)
   expect_equal(nrow(lower_report), 3)
   expect_equal(ncol(lower_report), 3)
   expect_equal(upper_report$absolute_count , c(1, 1, 1))
   expect_equal(lower_report$absolute_count , c(1, 1, 1))

   # proteingroup_precursor connection
   #upper
   upper_plot <- analyze_connected_levels(input_df = data, connected_levels = "mod.peptides_precursor",count_level = "upper", plot = TRUE, plot_characteristic = "relative")
   upper_report <- analyze_connected_levels(input_df = data, connected_levels = "mod.peptides_precursor",count_level = "upper", plot = FALSE)
   #lower
   lower_plot <- analyze_connected_levels(input_df = data, connected_levels = "mod.peptides_precursor",count_level = "lower", plot = TRUE, plot_characteristic = "relative")
   lower_report <- analyze_connected_levels(input_df = data, connected_levels = "mod.peptides_precursor",count_level = "lower", plot = FALSE)

   expect_error(analyze_connected_levels(input_df = data[,3:4], connected_levels = "mod.peptides_precursor",count_level = "upper", plot = TRUE, plot_characteristic = "relative"), "For connected levels - mod.peptides_precursor: traceR_connected_mod.pep_prec column must be present in submitted data.")
   expect_s3_class(upper_plot, "ggplot")
   expect_s3_class(lower_plot, "ggplot")
   expect_s3_class(upper_report, "tbl")
   expect_s3_class(lower_report, "tbl")
   expect_equal(nrow(upper_report), 2)
   expect_equal(ncol(upper_report), 3)
   expect_equal(nrow(lower_report), 2)
   expect_equal(ncol(lower_report), 3)
   expect_equal(upper_report$absolute_count , c(2, 1))
   expect_equal(lower_report$absolute_count , c(2, 1))

})
