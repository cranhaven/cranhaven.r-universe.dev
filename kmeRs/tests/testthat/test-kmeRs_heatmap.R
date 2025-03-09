test_that("Generate similarity matrix and display as a heatmap", {
  # Given hexamers
  kmers_given <- c("GATTACA", "ACAGATT", "GAATTAC", "GAAATCT", "CTATAGA", "GTACATA", "AACGATT")
  # Matrix calculation 
  kmers_mat <- kmeRs_similarity_matrix(q = kmers_given, submat = "BLOSUM62")
  # Score the matrix and sort by decreasing score 
  kmers_res <- kmeRs_score(kmers_mat)
  # Heatmap without sum column
  kmers_graph <- kmeRs_heatmap(kmers_res[, -8])  
  # Test
  expect_true(is.list(kmers_graph))
})