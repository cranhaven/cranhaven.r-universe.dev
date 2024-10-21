# tests/testthat/test-parse.R

test_that("parse_uniprot_data parses UniProt data correctly", {
  # Simulated UniProt API response for O88737
  test_uniprot_data <- list(
    uniProtkbId = "BSN_MOUSE",
    proteinDescription = list(
      recommendedName = list(
        fullName = list(value = "Protein bassoon")
      )
    ),
    genes = list(
      geneName = list(value = "Bsn")
    )
  )

  # Run the parse function
  parsed_result <- parse_uniprot_data(test_uniprot_data)

  # Check if the entry_name is correct
  expect_equal(parsed_result$entry_name, "BSN_MOUSE")

  # Check if the protein_name is correct
  expect_equal(parsed_result$protein_name, "Protein bassoon")

  # Check if the gene_name is correct
  expect_equal(parsed_result$gene_name, "Bsn")
})

test_that("parse_uniprot_data handles missing gene_name", {
  # Simulated UniProt API response without a gene_name
  test_uniprot_data_no_gene <- list(
    uniProtkbId = "BSN_MOUSE",
    proteinDescription = list(
      recommendedName = list(
        fullName = list(value = "Protein bassoon")
      )
    ),
    genes = NULL  # Missing gene_name
  )

  # Run the parse function
  parsed_result <- parse_uniprot_data(test_uniprot_data_no_gene)

  # Check if the entry_name is correct
  expect_equal(parsed_result$entry_name, "BSN_MOUSE")

  # Check if the protein_name is correct
  expect_equal(parsed_result$protein_name, "Protein bassoon")

  # Check if the gene_name is correctly set to NA
  expect_true(is.na(parsed_result$gene_name))
})
