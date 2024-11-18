test_that('physeq_format runs',{
  data(physeq_S2D2)
  x = physeq_format(physeq_S2D2)
  expect_is(x, 'phyloseq')
  expect_equal(x %>% sample_data %>% colnames,
               physeq_S2D2 %>% sample_data %>% colnames)

  # should NOT be correctly formatted for HTSSIP
  library(phyloseq)
  data(GlobalPatterns)
  expect_error(physeq_format(GlobalPatterns))
})
