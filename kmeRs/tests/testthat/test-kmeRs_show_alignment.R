test_that("multiplication works", {
  # Show example alignment
  kmeRs_align <- kmeRs_show_alignment(kmer_A = "AAATTTCCCGGG", kmer_B = "TCACCC", submat = "BLOSUM62")
  # Test
  expect_true(is.data.frame(kmeRs_align))
})
