# tests/testthat/test-compare.R

test_that("compare_tibbles_uniprot detects changes correctly", {
  # Original input tibble
  input_data <- tibble::tibble(
    id = c(1, 2),
    species = c("mouse", "rat"),
    sample_type = c("brain", "liver"),
    accession = c("O88737", "Q9R064"),
    accession_source = c("UniProt", "UniProt")
  )

  # Processed data (simulating output from process_tibble_uniprot)
  updated_data <- tibble::tibble(
    id = c(1, 2),
    species = c("mouse", "rat"),
    sample_type = c("brain", "liver"),
    accession = c("O88737", "Q9R064"),
    accession_source = c("UniProt", "UniProt"),
    entry_name = c("BSN_MOUSE", "GORS2_RAT"),
    protein_name = c("Protein bassoon", "Golgi reassembly-stacking protein 2"),
    gene_name = c("Bsn", "Gorasp2")
  )

  # Capture the messages from the comparison function
  output <- compare_tibbles_uniprot(input_data, updated_data)

  # Expect the specific update messages
  expect_true(any(grepl("entry_name updated from NA to BSN_MOUSE", output)))
  expect_true(any(grepl("protein_name updated from NA to Protein bassoon", output)))
  expect_true(any(grepl("gene_name updated from NA to Bsn", output)))

  # Test for rows without changes
  identical_data <- updated_data
  output_no_changes <- compare_tibbles_uniprot(updated_data, identical_data)

  expect_true(any(grepl("No changes detected", output_no_changes)))
})

