test_that('Total sum scaling works',{
  df = data.frame(1:10, 2:11)
  # margin = 2
  df_t = tss(df)
  expect_is(df_t, 'data.frame')
  expect_equal(unique(apply(df_t, 2, sum)), 1)

  # margin = 1
  df_t = tss(df, MARGIN=1)
  expect_is(df_t, 'data.frame')
  expect_equal(unique(apply(df_t, 1, sum)), 1)

  # scaling is static
  expect_true(all(tss(tss(df)) == tss(df)))
})


test_that('qPCR OTU table transformation',{
  # qPCR simulation
  # control_mean_fun = function(x) dnorm(x, mean=1.70, sd=0.01) * 1e8
  # control_sd_fun = function(x) control_mean_fun(x) / 3
  # treat_mean_fun = function(x) dnorm(x, mean=1.75, sd=0.01) * 1e8
  # treat_sd_fun = function(x) treat_mean_fun(x) / 3
  #
  # physeq = physeq_S2D2_l[[1]]
  # qPCR = qPCR_sim(physeq,
  #                    control_expr='Substrate=="12C-Con"',
  #                    control_mean_fun=control_mean_fun,
  #                    control_sd_fun=control_sd_fun,
  #                    treat_mean_fun=treat_mean_fun,
  #                    treat_sd_fun=treat_sd_fun)

  # OTU table transformation
  #physeq_t = OTU_qPCR_trans(physeq, qPCR)
  #expect_is(physeq_t, 'phyloseq')

  # OTU table transformation
  physeq_rep3_t = OTU_qPCR_trans(physeq_rep3, physeq_rep3_qPCR)
  X = physeq_rep3 %>% otu_table %>% colnames
  Y = physeq_rep3_t %>% otu_table %>% colnames
  expect_true(all(X %in% Y))
  expect_is(physeq_rep3_t, 'phyloseq')
  expect_equal(physeq_rep3_t %>% otu_table %>% min, 0)
  expect_false(physeq_rep3_t %>% otu_table %>% is.na() %>% any)
})
