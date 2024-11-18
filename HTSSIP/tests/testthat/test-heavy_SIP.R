# test_file('~/dev/HTSSIP/tests/testthat/test-heavy_SIP.R')

test_that('heavy-SIP runs on replicated dataset with default settings',{
  # Calculating heavy-SIP on replicated dataset
  data(physeq_rep3)
  ## default
  res = heavy_SIP(physeq_rep3,
                  ex="Treatment=='12C-Con'")
  expect_is(res, 'data.frame')
  expect_lte(nrow(res), phyloseq::ntaxa(physeq_rep3))
  ## H-v-L
  res = heavy_SIP(physeq_rep3,
                  ex="Treatment=='12C-Con'",
                  comparison='H-v-L')
  expect_is(res, 'data.frame')
  expect_lte(nrow(res), phyloseq::ntaxa(physeq_rep3))
  ## H-v-H
  res = heavy_SIP(physeq_rep3,
                  ex="Treatment=='12C-Con'",
                  comparison='H-v-H')
  expect_is(res, 'data.frame')
  expect_lte(nrow(res), phyloseq::ntaxa(physeq_rep3))
})


test_that('heavy-SIP runs on non-rep dataset with default settings',{
  # Calculating heavy-SIP on replicated dataset
  data(physeq_S2D2_l)

  ## default
  res = heavy_SIP(physeq_S2D2_l[[1]], ex="Substrate=='12C-Con'")
  expect_is(res, 'data.frame')
  expect_lte(nrow(res), phyloseq::ntaxa(physeq_S2D2_l[[1]]))
  ## H-v-L
  res = heavy_SIP(physeq_S2D2_l[[1]],
                  ex="Substrate=='12C-Con'",
                  comparison='H-v-L')
  expect_is(res, 'data.frame')
  expect_lte(nrow(res), phyloseq::ntaxa(physeq_S2D2_l[[1]]))
  ## H-v-H
  res = heavy_SIP(physeq_S2D2_l[[1]],
                  ex="Substrate=='12C-Con'",
                  comparison='H-v-H')
  expect_is(res, 'data.frame')
  expect_lte(nrow(res), phyloseq::ntaxa(physeq_S2D2_l[[1]]))
})

test_that('heavy-SIP runs on replicated dataset with t-test',{
  # Calculating heavy-SIP on replicated dataset

  ## default
  expect_error(heavy_SIP(physeq_rep3,
               ex="Treatment=='12C-Con'",
               hypo_test='t-test'))
  ## H-v-L
  res = heavy_SIP(physeq_rep3,
                  ex="Treatment=='12C-Con'",
                  comparison='H-v-L',
                  hypo_test='t-test')
  expect_is(res, 'data.frame')
  expect_lte(nrow(res), phyloseq::ntaxa(physeq_rep3))
  ## H-v-H
  res = heavy_SIP(physeq_rep3,
                  ex="Treatment=='12C-Con'",
                  comparison='H-v-H',
                  hypo_test='t-test')
  expect_is(res, 'data.frame')
  expect_lte(nrow(res), phyloseq::ntaxa(physeq_rep3))
})


test_that('heavy-SIP runs on replicated dataset with wilcox',{
  # Calculating heavy-SIP on replicated dataset

  ## default
  expect_error(heavy_SIP(physeq_rep3,
                         ex="Treatment=='12C-Con'",
                         hypo_test='wilcox'))
  ## H-v-L
  res = heavy_SIP(physeq_rep3,
                  ex="Treatment=='12C-Con'",
                  comparison='H-v-L',
                  hypo_test='wilcox')
  expect_is(res, 'data.frame')
  expect_lte(nrow(res), phyloseq::ntaxa(physeq_rep3))
  ## H-v-H
  res = heavy_SIP(physeq_rep3,
                  ex="Treatment=='12C-Con'",
                  comparison='H-v-H',
                  hypo_test='wilcox')
  expect_is(res, 'data.frame')
  expect_lte(nrow(res), phyloseq::ntaxa(physeq_rep3))
})
