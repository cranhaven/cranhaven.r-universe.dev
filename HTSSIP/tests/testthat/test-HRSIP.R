test_that('HRSIP runs with default',{
  ## basic call
  df_l2fc = HRSIP(physeq_S2D2_l[[1]],
                  sparsity_threshold = c(0,0.15,0.3),  # limited thresholds to speed up tests
                  design=~Substrate)
  expect_is(df_l2fc, 'data.frame')
  expect_gt(nrow(df_l2fc), 0)

  df_l2fc_s = df_l2fc %>%
    dplyr::group_by(sparsity_threshold,
                    density_min, density_max) %>%
    dplyr::summarize(n = n())
  expect_gt(nrow(df_l2fc_s), 1)
})

test_that('HRSIP runs padj_cutoff',{
  skip_on_cran()

  ## basic call
  df_l2fc = HRSIP(physeq_S2D2_l[[1]],
                  design=~Substrate,
                  sparsity_threshold = c(0,0.15,0.3), # limited thresholds to speed up tests
                  padj_cutoff=0.1)
  expect_is(df_l2fc, 'data.frame')
  expect_gt(nrow(df_l2fc), 0)

  df_l2fc_s = df_l2fc %>%
    dplyr::group_by(sparsity_threshold,
                    density_min, density_max) %>%
    dplyr::summarize(n = n())
  expect_equal(nrow(df_l2fc_s), 1)
})

test_that('MW-HR-SIP runs with default & parallel ',{
  skip_on_cran()

  doParallel::registerDoParallel(2)
  windows = data.frame(density_min=c(1.70, 1.72), density_max=c(1.73, 1.75))
  df_l2fc = HRSIP(physeq_S2D2_l[[1]],
                  design=~Substrate,
                  sparsity_threshold = c(0,0.15,0.3),  # limited thresholds to speed up tests
                  density_windows=windows,
                  parallel=TRUE)
  expect_is(df_l2fc, 'data.frame')
  expect_gt(nrow(df_l2fc), 0)

  df_l2fc_s = df_l2fc %>%
    dplyr::group_by(sparsity_threshold,
                    density_min, density_max) %>%
    dplyr::summarize(n = n())
  expect_gt(nrow(df_l2fc_s), 1)
})


test_that('MW-HR-SIP runs with padj_cutoff',{
  skip_on_cran()

  doParallel::registerDoParallel(2)
  windows = data.frame(density_min=c(1.70, 1.72), density_max=c(1.73, 1.75))
  df_l2fc = HRSIP(physeq_S2D2_l[[1]],
                  design=~Substrate,
                  density_windows=windows,
                  padj_cutoff=0.1,
                  parallel=TRUE)
  expect_is(df_l2fc, 'data.frame')
  expect_gt(nrow(df_l2fc), 0)

  df_l2fc_s = df_l2fc %>%
    dplyr::group_by(sparsity_threshold, density_min, density_max) %>%
    dplyr::summarize(n = n())
  expect_equal(nrow(df_l2fc_s), 2)
})
