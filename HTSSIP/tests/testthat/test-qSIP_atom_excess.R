# test_file('~/dev/HTSSIP/tests/testthat/test-qSIP_atom_excess.R')

test_that('qSIP_BD_shift working', {
  skip_on_cran()

  data(physeq_rep3)
  data(physeq_rep3_qPCR)
  physeq_rep3_t = OTU_qPCR_trans(physeq_rep3, physeq_rep3_qPCR)

  # BD shift (Z) & atom excess (A)
  atomX = qSIP_atom_excess(physeq_rep3_t,
                           control_expr='Treatment=="12C-Con"',
                           treatment_rep='Replicate')

  # output object formatted correctly
  expect_is(atomX, 'list')
  expect_false(is.null(atomX$A$Z))
  expect_false(is.null(atomX$A$A))
  expect_equal(atomX$A$OTU %>% length,
               atomX$A$OTU %>% unique %>% length)

  ## W variance among gradient replicates is > 0
  min_var = atomX$W %>%
    dplyr::group_by(IS_CONTROL, OTU) %>%
    dplyr::summarize(W_var = var(W)) %>%
    .$W_var %>% min
  expect_gt(min_var, 0)
})

test_that('bootstrap iteration is working', {
  data(physeq_rep3)
  data(physeq_rep3_qPCR)
  physeq_rep3_t = OTU_qPCR_trans(physeq_rep3, physeq_rep3_qPCR)

  # BD shift (Z) & atom excess (A)
  atomX = qSIP_atom_excess(physeq_rep3_t,
                           control_expr='Treatment=="12C-Con"',
                           treatment_rep='Replicate')

  # bootstrap
  atomX_boot = .qSIP_bootstrap(atomX)
  expect_is(atomX_boot, 'data.frame')
  expect_false(is.null(atomX_boot$Z))
  expect_false(is.null(atomX_boot$A))
  expect_gt(atomX_boot$bootstrap_id %>% min, 0)
})

test_that('bootstrap in parallel working', {
  skip_on_cran()

  data(physeq_rep3)
  data(physeq_rep3_qPCR)
  physeq_rep3_t = OTU_qPCR_trans(physeq_rep3, physeq_rep3_qPCR)

  # BD shift (Z) & atom excess (A)
  atomX = qSIP_atom_excess(physeq_rep3_t,
                           control_expr='Treatment=="12C-Con"',
                           treatment_rep='Replicate')

  doParallel::registerDoParallel(2)
  # qSIP with bootstrapping
  df_atomX_boot = qSIP_bootstrap(atomX, parallel=TRUE, n_boot=20)
  # output formatted correctly
  expect_is(df_atomX_boot, 'data.frame')
  # gte zero CI ranges
  CI_diff = df_atomX_boot$A_CI_high - df_atomX_boot$A_CI_low
  expect_gt(CI_diff %>% min, 0)
  # A in CI(A)
  CI_in = df_atomX_boot$A_CI_high >= df_atomX_boot$A &
          df_atomX_boot$A_CI_low <= df_atomX_boot$A
  expect_true(CI_in %>% all)
})

