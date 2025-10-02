test_that("frequencies are shifted correctly", {

  test_data <- data.frame(selected_parent_alt_af = c(0.25, 0.25, 0.25, 0.75, 0.75, 0.75 ), backcrossed_parent_alt_af = c( 0.75, 0.75, 0.75,0.25, 0.25, 0.25 ), offspring_alt_af = c(0.0, 0.5, 1.0, 0.0, 0.5, 1.0 ))
  test_data$start <- seq(1, nrow(test_data))
  test_data %<>% dplyr::mutate(seqnames = "chr2L", end=start, width =1, strand = "*", name = 0, score = 0, ref = "A", alt = "T" , selected_parent_count=1, backcrossed_parent_count=1, offspring_count=1)  %>% GenomicRanges::GRanges()

  correct_answer <- c(0.5,0.0,-0.5,-0.5, 0.0, 0.5)
  expect_equal(freqShifter(test_data)$mean_oriented_shift, correct_answer)

})


