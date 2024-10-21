# tests/testthat/test-process.R

test_that("process_tibble_uniprot processes correctly", {

  # Test data (similar to what we had before)
  test_data <- tibble::tibble(
    id = c(1, 78, 83, 87, 94, 96, 100, 102, 106, 109, 116, 118, 140, 143, 148, 152, 163, 175, 185, 188),
    species = c("mouse", "mouse", "rat", "mouse", "rat", "mouse", "mouse", "mouse", "rat", "mouse", "rat",
                "mouse", "mouse", "human", "mouse", "rat", "rat", "rat", "rat", "rat"),
    sample_type = c("brain", "brain", "brain", "brain", "brain", "brain", "brain", "brain", "spinal cord",
                    "brain", "brain", "brain", "brain", "Sf9 cells", "brain", "liver", "liver", "liver", "liver", "liver"),
    accession = c("O88737", "O35927", "Q9R064", "P51611", "Q62784", "P46660", "P59759", "P08551", "P19527",
                  "P08553", "Q4KLH5", "Q9QYX7", "Q62417", "P11831", "O88737", "P05696", "P68403", "P63319", "P09215", "Q6DUV1"),
    accession_source = c("OtherDB", "UniProt", "UniProt", "UniProt", "UniProt", "UniProt", "UniProt", "UniProt", "UniProt",
                         "UniProt", "UniProt", "UniProt", "UniProt", "UniProt", "UniProt", "UniProt", "UniProt", "UniProt", "UniProt", "UniProt"),
    entry_name = c("BSN_MOUSE", "CTND2_MOUSE", "GORS2_RAT", "HCFC1_MESAU", NA, "AINX_MOUSE",
                   "MRTFB_MOUSE", NA, "NFL_RAT", "NFM_MOUSE", "AGFG1_RAT", NA,
                   "SRBS1_MOUSE", NA, "BSN_MOUSE", "KPCA_RAT", "KPCB_RAT", "KPCG_RAT",
                   "KPCD_RAT", NA),
    protein_name = c("Protein bassoon", "Catenin delta-2", "Golgi reassembly-stacking protein2",
                     "Host cell factor1", "Inositol polyphosphate-4-phosphatase type I A",
                     "Alpha-internexin", "Myocardin-related transcription factorB", NA,
                     "Neurofilament light polypeptide", "Neurofilament medium polypeptide", "Arf-GAP domain and FG repeat-containing protein 1",
                     NA, "Sorbin and SH3 domain-containing protein 1", NA, "Protein bassoon",
                     "Protein kinase C alpha type", "Protein kinase C beta type", "Protein kinase C gamma type",
                     "Protein kinase C delta type", NA),
    gene_name = c("Bsn", "Ctnnd2", "Gorasp2", "HCFC1", "Inpp4a", "Ina", "Mrtfb", NA, "Nefl", "Nefm",
                  "Agfg1", NA, "Sorbs1", "SRF", "Bsn", "Prkca", "Prkcb", "Prkcg", "Prkcd", NA)
  )

  # Run the process_tibble_uniprot function
  result_data <- process_tibble_uniprot(test_data)

  # Validate the result is still a tibble and has correct dimensions
  expect_s3_class(result_data, "tbl_df")
  expect_equal(nrow(result_data), nrow(test_data))
  expect_equal(ncol(result_data), ncol(test_data))

  # Validate that some known rows have been updated
  expect_true(!is.na(result_data$entry_name[2]))  # Since row 2 is UniProt

})
