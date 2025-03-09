context("test_trace")

#connect_traceR_levels
test_that("connect_traceR_levels is succesful", {

    data <- tibble::tibble(
       "traceR_traced_proteinGroups" = c("common", "common", "unique"),
       "traceR_traced_mod.peptides" = c("common", "unique", "common"),
       "traceR_traced_precursor" = c("common", "unique", "common")
    )

   #proteingroup level
   output <- connect_traceR_levels(input_df = data, level = "proteinGroups")

   expect_error(connect_traceR_levels(input_df = data[, 2:3], level = "proteinGroups"), "For proteinGroups - level: traceR_traced_precursor and traceR_traced_proteinGroups column must be present in submitted data.")
   expect_s3_class(output, "tbl")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 4)
   expect_equal(output$traceR_connected_pg_prec , c("common_common", "common_unique", "unique_common"))

   #modified peptide level
   output <- connect_traceR_levels(input_df = data, level = "modified_peptides")

   expect_error(connect_traceR_levels(input_df = data[, 1:2], level = "modified_peptides"), "For modified_peptides - level: traceR_traced_mod.peptides and traceR_traced_precursor column must be present in submitted data.")
   expect_s3_class(output, "tbl")
   expect_equal(nrow(output), 3)
   expect_equal(ncol(output), 4)
   expect_equal(output$traceR_connected_mod.pep_prec , c("common_common", "unique_unique", "common_common"))

})

#trace_level
test_that("trace_level is succesful", {

  data1 <- tibble::tibble(
    "traceR_proteinGroups" = c("P02768", "P02671", "Q92496", "DummyProt"),
    "traceR_mod.peptides" = c("AAC(UniMod:4)LLPK", "RLEVDIDIK", "EGIVEYPR", "ALTDM(DummyModification)PQMK"),
    "traceR_mod.peptides_unknownMods" = c(FALSE, FALSE, FALSE, TRUE),
    "traceR_precursor" = c("AAC(UniMod:4)LLPK1", "RLEVDIDIK2", "EGIVEYPR2", "ALTDM(DummyModification)PQMK3" ),
    "traceR_precursor_unknownMods" = c(FALSE, FALSE, FALSE, TRUE)
  )

  data2 <- tibble::tibble(
    "traceR_proteinGroups" = c("P02768", "Q02985", "P02671"),
    "traceR_mod.peptides" = c("AAC(UniMod:4)LLPK", "EGIVEYPR", "M(UniMod:35)KPVPDLVPGNFK"),
    "traceR_mod.peptides_unknownMods" = c(FALSE, FALSE, FALSE),
    "traceR_precursor" = c("AAC(UniMod:4)LLPK1", "EGIVEYPR2", "M(UniMod:35)KPVPDLVPGNFK2"),
    "traceR_precursor_unknownMods" = c(FALSE, FALSE, FALSE)
  )

  #precursor level
  output <- trace_level(input_df1 = data1, input_df2 = data2, analysis_name1 = "data1", analysis_name2 = "data2", level = "precursor", filter_unknown_mods = FALSE)
  output_filtered <- trace_level(input_df1 = data1, input_df2 = data2, analysis_name1 = "data1", analysis_name2 = "data2", level = "precursor", filter_unknown_mods = TRUE)

  expect_error(trace_level(input_df1 = data1[,1:2], input_df2 = data2, analysis_name1 = "data1", analysis_name2 = "data2", level = "precursor", filter_unknown_mods = FALSE), "For precursor level: traceR_precursor column must be present in submitted data.")
  expect_type(output, "list")
  expect_equal(nrow(output[[1]]), 4)
  expect_equal(ncol(output[[1]]), 6)
  expect_equal(output[[1]]$traceR_traced_precursor , c("common", "common", "unique", "unique"))

  expect_type(output_filtered, "list")
  expect_equal(nrow(output_filtered[[1]]), 3)
  expect_equal(ncol(output_filtered[[1]]), 6)
  expect_equal(output_filtered[[1]]$traceR_traced_precursor , c("common", "common", "unique"))

  #modified peptide level
  output <- trace_level(input_df1 = data1, input_df2 = data2, analysis_name1 = "data1", analysis_name2 = "data2", level = "modified_peptides", filter_unknown_mods = FALSE)
  output_filtered <- trace_level(input_df1 = data1, input_df2 = data2, analysis_name1 = "data1", analysis_name2 = "data2", level = "modified_peptides", filter_unknown_mods = TRUE)

  expect_error(trace_level(input_df1 = data1[, 1], input_df2 = data2, analysis_name1 = "data1", analysis_name2 = "data2", level = "modified_peptides", filter_unknown_mods = FALSE), "For modified peptide level: traceR_mod.peptides column must be present in submitted data.")
  expect_type(output, "list")
  expect_equal(nrow(output[[1]]), 4)
  expect_equal(ncol(output[[1]]), 6)
  expect_equal(output[[1]]$traceR_traced_mod.peptides , c("common", "common", "unique", "unique"))

  expect_type(output_filtered, "list")
  expect_equal(nrow(output_filtered[[1]]), 3)
  expect_equal(ncol(output_filtered[[1]]), 6)
  expect_equal(output_filtered[[1]]$traceR_traced_mod.peptides , c("common", "common", "unique"))

  #proteingroup level
  output <- trace_level(input_df1 = data1, input_df2 = data2, analysis_name1 = "data1", analysis_name2 = "data2", level = "proteinGroups", filter_unknown_mods = FALSE)
  output_filtered <- trace_level(input_df1 = data1, input_df2 = data2, analysis_name1 = "data1", analysis_name2 = "data2", level = "proteinGroups", filter_unknown_mods = TRUE)

  expect_error(trace_level(input_df1 = data1[, 2], input_df2 = data2, analysis_name1 = "data1", analysis_name2 = "data2", level = "proteinGroups", filter_unknown_mods = FALSE), "For proteinGroups level: traceR_proteinGroups column must be present in submitted data.")
  expect_type(output, "list")
  expect_equal(nrow(output[[1]]), 4)
  expect_equal(ncol(output[[1]]), 6)
  expect_equal(output[[1]]$traceR_traced_proteinGroups , c("common", "common", "unique", "unique"))

  expect_type(output_filtered, "list")
  expect_equal(nrow(output_filtered[[1]]), 3)
  expect_equal(ncol(output_filtered[[1]]), 6)
  expect_equal(output_filtered[[1]]$traceR_traced_proteinGroups , c("common", "common", "unique"))

})

#trace_all_levels
test_that("trace_all_level is succesful", {

  data1 <- tibble::tibble(
    "traceR_proteinGroups" = c("P02768", "P02671", "Q92496", "DummyProt"),
    "traceR_mod.peptides" = c("AAC(UniMod:4)LLPK", "RLEVDIDIK", "EGIVEYPR", "ALTDM(DummyModification)PQMK"),
    "traceR_mod.peptides_unknownMods" = c(FALSE, FALSE, FALSE, TRUE),
    "traceR_precursor" = c("AAC(UniMod:4)LLPK1", "RLEVDIDIK2", "EGIVEYPR2", "ALTDM(DummyModification)PQMK3" ),
    "traceR_precursor_unknownMods" = c(FALSE, FALSE, FALSE, TRUE)
  )

  data2 <- tibble::tibble(
    "traceR_proteinGroups" = c("P02768", "Q02985", "P02671"),
    "traceR_mod.peptides" = c("AAC(UniMod:4)LLPK", "EGIVEYPR", "M(UniMod:35)KPVPDLVPGNFK"),
    "traceR_mod.peptides_unknownMods" = c(FALSE, FALSE, FALSE),
    "traceR_precursor" = c("AAC(UniMod:4)LLPK1", "EGIVEYPR2", "M(UniMod:35)KPVPDLVPGNFK2"),
    "traceR_precursor_unknownMods" = c(FALSE, FALSE, FALSE)
  )

  output <- trace_all_levels(input_df1 = data1, input_df2 = data2, analysis_name1 = "data1", analysis_name2 = "data2", filter_unknown_mods = FALSE)
  output_filtered <- trace_all_levels(input_df1 = data1, input_df2 = data2, analysis_name1 = "data1", analysis_name2 = "data2", filter_unknown_mods = TRUE)

  expect_type(output, "list")
  expect_equal(nrow(output[[1]]), 4)
  expect_equal(ncol(output[[1]]), 8)
  expect_equal(output[[1]]$traceR_traced_proteinGroups , c("common", "common", "unique", "unique"))

  expect_type(output_filtered, "list")
  expect_equal(nrow(output_filtered[[1]]), 3)
  expect_equal(ncol(output_filtered[[1]]), 8)
  expect_equal(output_filtered[[1]]$traceR_traced_proteinGroups , c("common", "common", "unique"))

})

#trace_unique_common_pg
test_that("trace_unique_common_pg is succesful", {

  data1 <- tibble::tibble(
    "traceR_connected_pg_prec" = c("common_common", "common_unique", "unique_common", "unique_common"),
    "traceR_proteinGroups" = c("P02768", "P02671", "Q92496", "P04433"),
    "traceR_precursor" = c("AAC(UniMod:4)LLPK1", "RLEVDIDIK2", "EGIVEYPR2", "ASQSVSSYLAWYQQK2"),
  )

  data2 <- tibble::tibble(
    "traceR_connected_pg_prec" = c("common_common", "common_unique", "unique_common", "unique_common"),
    "traceR_proteinGroups" = c("P02768", "P02671", "Q02985", "A0A0A0MRZ8;P04433"),
    "traceR_precursor" = c("AAC(UniMod:4)LLPK1", "M(UniMod:35)KPVPDLVPGNFK2", "EGIVEYPR2", "ASQSVSSYLAWYQQK2"),
  )

  output <- trace_unique_common_pg(input_df1 = data1, input_df2 = data2, analysis_name1 = "data1", analysis_name2 = "data2", string_analysis = FALSE)
  output_filtered <- trace_unique_common_pg(input_df1 = data1, input_df2 = data2, analysis_name1 = "data1", analysis_name2 = "data2", string_analysis = TRUE)

  expect_error(trace_unique_common_pg(input_df1 = data1[, 2:3], input_df2 = data2, analysis_name1 = "data1", analysis_name2 = "data2", string_analysis = FALSE), "For connected_levels proteinGroup_precursor: traceR_connected_pg_prec column must be present in submitted data.")
  expect_s3_class(output, "tbl")
  expect_equal(nrow(output), 2)
  expect_equal(ncol(output), 3)
  expect_equal(output$traceR_proteinGroups_data2 , c("Q02985", "A0A0A0MRZ8;P04433"))

  expect_type(output_filtered, "list")
  expect_equal(nrow(output_filtered), 1)
  expect_equal(ncol(output_filtered), 3)
  expect_equal(output_filtered$traceR_proteinGroups_data2 , c("Q02985"))

})
