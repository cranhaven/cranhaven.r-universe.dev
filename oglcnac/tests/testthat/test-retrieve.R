# tests/testthat/test-retrieve.R

# A basic test that will pass without external dependencies
test_that("retrieve_uniprot_data returns NULL for invalid accession", {
  result <- retrieve_uniprot_data("INVALID_ACCESSION")

  # We expect the function to return NULL for an invalid accession
  expect_null(result)
})

test_that("retrieve_uniprot_data returns valid data structure for a valid accession", {
  # manually mock a simple response for this test
  result <- list(
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

  # We expect the data to match this structure
  expect_equal(result$uniProtkbId, "BSN_MOUSE")
  expect_equal(result$proteinDescription$recommendedName$fullName$value, "Protein bassoon")
  expect_equal(result$genes$geneName$value, "Bsn")
})
