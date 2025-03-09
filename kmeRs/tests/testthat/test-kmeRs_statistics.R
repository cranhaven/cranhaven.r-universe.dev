test_that("Calculate basic statistics for the matrix", {
  # Given hexamers
  kmers_given <- c("GATTACA", "ACAGATT", "GAATTAC", "GAAATCT", "CTATAGA", "GTACATA", "AACGATT")
  # Matrix calculation 
  kmers_mat <- kmeRs_similarity_matrix(q = kmers_given, submat = "BLOSUM62")
  # Score the matrix and sort by decreasing score 
  kmers_res <- kmeRs_score(kmers_mat)
  # Calculate stats   
  kmers_stats <- kmeRs_statistics(kmers_res)
  # Test
  expect_true(is.data.frame(kmers_stats))
})