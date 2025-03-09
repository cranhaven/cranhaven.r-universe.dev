test_that("Generate BLOSUM62 matrix", {
  # Test
  expect_true(is.data.frame(kmeRs_similarity_matrix(submat = "BLOSUM62")))
})

test_that("Generate similarity matrix for given k-mer", {
  # Given hexamers
  kmers_given <- c("GATTACA", "ACAGATT", "GAATTAC", "GAAATCT", "CTATAGA", "GTACATA", "AACGATT")
  # Matrix calculation 
  kmers_mat <- kmeRs_similarity_matrix(q = c("GATTACA"), x = kmers_given , submat = "BLOSUM62") 
  # Test
  expect_true(is.data.frame(kmers_mat))
})

test_that("Generate similarity matrix for given k-mers", {
  # Given hexamers
  kmers_given <- c("GATTACA", "ACAGATT", "GAATTAC", "GAAATCT", "CTATAGA", "GTACATA", "AACGATT")
  # Matrix calculation 
  kmers_mat <- kmeRs_similarity_matrix(q = kmers_given, submat = "BLOSUM62")
  # Score the matrix and sort by decreasing score 
  kmers_res <- kmeRs_score(kmers_mat)
  # Test
  expect_true(is.data.frame(kmers_mat))
})
