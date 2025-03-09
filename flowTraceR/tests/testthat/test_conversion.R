context("test_conversion")

#*convert_precursor*=====
#MaxQuant
test_that("convert_precursor with MaxQuant output is succesful", {

  data <- tibble::tibble(
    "Modified sequence" = c("_AACLLPK_", "_ALTDM(Oxidation (M))PQM(Oxidation (M))R_", "ALTDM(Dummy_Modification)PQMK"),
    Charge = c(2,2,3)
  )

  output <- convert_precursor(input_df = data, software = "MaxQuant")

  expect_error(convert_precursor(input_df = data[,1], software = "MaxQuant"), "For MaxQuant input: Modified sequence column and Charge column need to be present in submitted data.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 3)
  expect_equal(ncol(output), 5)
  expect_equal(output$traceR_precursor_unknownMods, c(FALSE, FALSE, TRUE))

})

#PD
test_that("convert_precursor with PD output is succesful", {

  data <- tibble::tibble(
    "Annotated Sequence" = c("AAcLLPK", "AADDTWEPFASGK", "DYELLcLDGTRKPVEEYANcHLAR"),
    Modifications = c("C3(Carbamidomethyl)", "", "C6(Carbamidomethyl); C20(Dummy_Modification)"),
    Charge = c(2,2,3)
  )

  output <- convert_precursor(input_df = data, software = "PD")

  expect_error(convert_precursor(input_df = data[,1], software = "PD"), "For PD input: Annotated Sequence column, Charge column and Modifications column need to be present in submitted data.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 3)
  expect_equal(ncol(output), 6)
  expect_equal(output$traceR_precursor_unknownMods, c(FALSE, FALSE, TRUE))

})

#DIA-NN
test_that("convert_precursor with DIA-NN output is succesful", {

  data <- tibble::tibble(
    "Dummy_column" = c("A", "B", "C"),
    "Precursor.Id" = c("AAAATGTIFTFR2", "AEDTAVYYC(UniMod:4)AK2", "AAC(Dummy_Modification)LLPK2")
  )

  output <- convert_precursor(input_df = data, software = "DIA-NN")

  expect_error(convert_precursor(input_df = data[,1], software = "DIA-NN"), "For DIA-NN input: Precursor.Id column needs to be present in submitted data.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 3)
  expect_equal(ncol(output), 4)
  expect_equal(output$traceR_precursor_unknownMods, c(FALSE, FALSE, TRUE))

})

#Spectronaut
test_that("convert_precursor with Spectronaut output is succesful", {

  data <- tibble::tibble(
    "Dummy_column" = c("A", "B", "C"),
    "EG.PrecursorId" = c("_AEDTAVYYC[Carbamidomethyl (C)]AK_.2", "_M[Oxidation (M)]KPVPDLVPGNFK_.2", "_M[Dummy_Modification (M)]KPVPDLVPGNFK_.3")
  )

  output <- convert_precursor(input_df = data, software = "Spectronaut")

  expect_error(convert_precursor(input_df = data[,1], software = "Spectronaut"), "For Spectronaut input: EG.PrecursorId column needs to be present in submitted data.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 3)
  expect_equal(ncol(output), 4)
  expect_equal(output$traceR_precursor_unknownMods, c(FALSE, FALSE, TRUE))

})

#*convert_modified_peptides*=====
#MaxQuant
test_that("convert_modified_peptides with MaxQuant output is succesful", {

  data <- tibble::tibble(
    "Modified sequence" = c("_AACLLPK_", "_ALTDM(Oxidation (M))PQM(Oxidation (M))R_", "ALTDM(Dummy_Modification)PQMK"),
    Charge = c(2,2,3)
  )

  output <- convert_modified_peptides(input_df = data, software = "MaxQuant")

  expect_error(convert_modified_peptides(input_df = data[,1], software = "MaxQuant"), "For MaxQuant input: Modified sequence column and Charge column need to be present in submitted data.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 3)
  expect_equal(ncol(output), 5)
  expect_equal(output$traceR_mod.peptides_unknownMods, c(FALSE, FALSE, TRUE))

})

#PD
test_that("convert_modified_peptides with PD output is succesful", {

  data <- tibble::tibble(
    "Annotated Sequence" = c("AAcLLPK", "AADDTWEPFASGK", "DYELLcLDGTRKPVEEYANcHLAR"),
    Modifications = c("C3(Carbamidomethyl)", "", "C6(Carbamidomethyl); C20(Dummy_Modification)"),
    Charge = c(2,2,3)
  )

  output <- convert_modified_peptides(input_df = data, software = "PD")

  expect_error(convert_modified_peptides(input_df = data[,1], software = "PD"), "For PD input: Annotated Sequence column, Charge column and Modifications column need to be present in submitted data.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 3)
  expect_equal(ncol(output), 6)
  expect_equal(output$traceR_mod.peptides_unknownMods, c(FALSE, FALSE, TRUE))

})

#DIA-NN
test_that("convert_modified_peptides with DIA-NN output is succesful", {

  data <- tibble::tibble(
    "Dummy_column" = c("A", "B", "C"),
    "Precursor.Id" = c("AAAATGTIFTFR2", "AEDTAVYYC(UniMod:4)AK2", "AAC(Dummy_Modification)LLPK2")
  )

  output <- convert_modified_peptides(input_df = data, software = "DIA-NN")

  expect_error(convert_modified_peptides(input_df = data[,1], software = "DIA-NN"), "For DIA-NN input: Precursor.Id column needs to be present in submitted data.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 3)
  expect_equal(ncol(output), 4)
  expect_equal(output$traceR_mod.peptides_unknownMods, c(FALSE, FALSE, TRUE))

})

#Spectronaut
test_that("convert_modified_peptides with Spectronaut output is succesful", {

  data <- tibble::tibble(
    "Dummy_column" = c("A", "B", "C"),
    "EG.PrecursorId" = c("_AEDTAVYYC[Carbamidomethyl (C)]AK_.2", "_M[Oxidation (M)]KPVPDLVPGNFK_.2", "_M[Dummy_Modification (M)]KPVPDLVPGNFK_.3")
  )

  output <- convert_modified_peptides(input_df = data, software = "Spectronaut")

  expect_error(convert_modified_peptides(input_df = data[,1], software = "Spectronaut"), "For Spectronaut input: EG.PrecursorId column needs to be present in submitted data.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 3)
  expect_equal(ncol(output), 4)
  expect_equal(output$traceR_mod.peptides_unknownMods, c(FALSE, FALSE, TRUE))

})

#*convert_proteingroups*=====
#MaxQuant
test_that("convert_proteingroups with MaxQuant output is succesful", {

  data <- tibble::tibble(
    "Dummy_column" = c("A", "B", "C"),
    "Protein IDs" = c("P01615;A0A087WW87", "P02768", "Q14624")
  )

  output <- convert_proteingroups(input_df = data, software = "MaxQuant")

  expect_error(convert_proteingroups(input_df = data[,1], software = "MaxQuant"), "For MaxQuant input: Protein IDs column must be present in submitted data.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 3)
  expect_equal(ncol(output), 3)
  expect_equal(output$traceR_proteinGroups, c("A0A087WW87;P01615", "P02768", "Q14624"))

})

#PD
test_that("convert_proteingroups with PD output is succesful", {

  data <- tibble::tibble(
    "Dummy_column" = c("A", "B", "C"),
    "Protein Accessions" = c("B4J1V0; A0A0BY9", "P02768", "Q14624")
  )

  output <- convert_proteingroups(input_df = data, software = "PD")

  expect_error(convert_proteingroups(input_df = data[,1], software = "PD"), "For PD input: Protein Accessions column must be present in submitted data.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 3)
  expect_equal(ncol(output), 3)
  expect_equal(output$traceR_proteinGroups, c("A0A0BY9;B4J1V0", "P02768", "Q14624"))

})

#DIA-NN
test_that("convert_proteingroups with DIA-NN output is succesful", {

  data <- tibble::tibble(
    "Dummy_column" = c("A", "B", "C"),
    "Protein.Group" = c("P0C0L4;P0C0L5", "P02768", "Q14624")
  )

  output <- convert_proteingroups(input_df = data, software = "DIA-NN")

  expect_error(convert_proteingroups(input_df = data[,1], software = "DIA-NN"), "For DIA-NN input: Protein.Group column must be present in submitted data.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 3)
  expect_equal(ncol(output), 3)
  expect_equal(output$traceR_proteinGroups, c("P0C0L4;P0C0L5", "P02768", "Q14624"))

})

#Spectronaut
test_that("convert_proteingroups with Spectronaut output is succesful", {

  data <- tibble::tibble(
    "Dummy_column" = c("A", "B", "C"),
    "PG.ProteinGroups" = c("A0A0B4J2D9;P0DP09", "P02768", "Q14624")
  )

  output <- convert_proteingroups(input_df = data, software = "Spectronaut")

  expect_error(convert_proteingroups(input_df = data[,1], software = "Spectronaut"), "For Spectronaut input: PG.ProteinGroups column must be present in submitted data.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 3)
  expect_equal(ncol(output), 3)
  expect_equal(output$traceR_proteinGroups, c("A0A0B4J2D9;P0DP09", "P02768", "Q14624"))

})

#*convert_all_levels*=====
#MaxQuant
test_that("convert_all_levels with MaxQuant output is succesful", {

  evidence <- tibble(
    "Modified sequence" = c("_AACLLPK_", "_ALTDM(Oxidation (M))PQM(Oxidation (M))R_", "ALTDM(Dummy_Modification)PQMK"),
    Charge = c(2,2,3),
    "Protein group IDs" = c("26", "86;17", "86;17")
  )

  proteingroups <- tibble(
    "Protein IDs" = c("A0A075B6P5;P01615;A0A087WW87;P01614;A0A075B6S6", "P02671", "P02672"),
    id = c(26, 86, 17)
  )

  output <- convert_all_levels(input_df = evidence, input_MQ_pg = proteingroups, software = "MaxQuant")

  expect_error(convert_all_levels(input_df = evidence[,1], input_MQ_pg = proteingroups, software = "MaxQuant"), "For MaxQuant input: Modified sequence column, Charge and Protein group IDs column need to be present in input_df.")
  expect_error(convert_all_levels(input_df = evidence, input_MQ_pg = proteingroups[,1], software = "MaxQuant"), "For MaxQuant input: Protein IDs and id column must be present in input_MQ_pg.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 5)
  expect_equal(ncol(output), 10)
  expect_equal(output$traceR_precursor_unknownMods , c(FALSE, FALSE, TRUE, FALSE, TRUE))

})

#PD
test_that("convert_all_levels with PD output is succesful", {

  data <- tibble::tibble(
    "Protein Accessions" = c("B4J1V0; A0A0BY9", "P02768", "Q14624"),
    "Annotated Sequence" = c("AAcLLPK", "AADDTWEPFASGK", "DYELLcLDGTRKPVEEYANcHLAR"),
    Modifications = c("C3(Carbamidomethyl)", "", "C6(Carbamidomethyl); C20(Dummy_Modification)"),
    Charge = c(2,2,3)
  )

  output <- convert_all_levels(input_df = data, software = "PD")

  expect_error(convert_all_levels(input_df = data[,2:4], software = "PD"), "For PD input: Protein Accessions column must be present in submitted data.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 3)
  expect_equal(ncol(output), 10)
  expect_equal(output$traceR_precursor_unknownMods , c(FALSE, FALSE, TRUE))

})

#DIA-NN
test_that("convert_all_levels with DIA-NN output is succesful", {

  data <- tibble::tibble(
    "Protein.Group" = c("P0C0L4;P0C0L5", "P02768", "Q14624"),
    "Precursor.Id" = c("AAAATGTIFTFR2", "AEDTAVYYC(UniMod:4)AK2", "AAC(Dummy_Modification)LLPK2")
  )

  output <- convert_all_levels(input_df = data, software = "DIA-NN")

  expect_error(convert_all_levels(input_df = data[,2], software = "DIA-NN"), "For DIA-NN input: Protein.Group column must be present in submitted data.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 3)
  expect_equal(ncol(output), 7)
  expect_equal(output$traceR_precursor_unknownMods , c(FALSE, FALSE, TRUE))

})

#Spectronaut
test_that("convert_all_levels with Spectronaut output is succesful", {

  data <- tibble::tibble(
    "PG.ProteinGroups" = c("A0A0B4J2D9;P0DP09", "P02768", "Q14624"),
    "EG.PrecursorId" = c("_AEDTAVYYC[Carbamidomethyl (C)]AK_.2", "_M[Oxidation (M)]KPVPDLVPGNFK_.2", "_M[Dummy_Modification (M)]KPVPDLVPGNFK_.3")
  )

  output <- convert_all_levels(input_df = data, software = "Spectronaut")

  expect_error(convert_all_levels(input_df = data[,2], software = "Spectronaut"), "For Spectronaut input: PG.ProteinGroups column must be present in submitted data.")
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 3)
  expect_equal(ncol(output), 7)
  expect_equal(output$traceR_precursor_unknownMods , c(FALSE, FALSE, TRUE))

})
