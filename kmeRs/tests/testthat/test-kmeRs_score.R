test_that("Generate similarity matrix for given k-mers", {
  # Given hexamers
  kmers_given <- c("GATTACA", "ACAGATT", "GAATTAC", "GAAATCT", "CTATAGA", "GTACATA", "AACGATT")
  # Matrix calculation 
  kmers_mat <- kmeRs_similarity_matrix(q = c("GATTACA"), x = kmers_given , submat = "BLOSUM62") 
  # Score and sort the matrix  
  kmers_res <- kmeRs_score(kmers_mat)
  # Test
  expect_true(is.data.frame(kmers_res))
})