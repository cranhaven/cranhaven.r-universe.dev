test_that('DESeq2_l2fc runs with default params',{
  df_l2fc = DESeq2_l2fc(physeq_S2D2_l[[1]],
                        sparsity_threshold=0.25,
                        density_min=1.71,
                        density_max=1.75,
                        design=~Substrate)

  expect_is(df_l2fc, 'data.frame')
  expect_gt(nrow(df_l2fc), 0)
  expect_equal(unique(df_l2fc$density_min)[1], 1.71)
  expect_equal(unique(df_l2fc$density_max)[1], 1.75)
})

test_that('DESeq2_l2fc runs with sparsity_apply=heavy',{
  df_l2fc = DESeq2_l2fc(physeq_S2D2_l[[1]],
                        sparsity_threshold=0.25,
                        density_min=1.71,
                        density_max=1.75,
                        design=~Substrate,
                        sparsity_apply='heavy')

  expect_is(df_l2fc, 'data.frame')
  expect_gt(nrow(df_l2fc), 0)
  expect_equal(unique(df_l2fc$density_min)[1], 1.71)
  expect_equal(unique(df_l2fc$density_max)[1], 1.75)
})
