# test_file('~/dev/HTSSIP/tests/testthat/test-delta_BD.R')

test_that('linear interpolation of abundances from BD values',{
  df = data.frame(
    Buoyant_density = seq(1.68, 1.76, 0.01),
    Count = rnorm(9, mean=1000, sd=100)
  )
  BD_min = min(df$Buoyant_density)
  BD_max = max(df$Buoyant_density)
  df_interp = lin_interp(df, n=20, BD_min=BD_min, BD_max=BD_max)
  expect_is(df_interp, 'data.frame')
  expect_equal(nrow(df_interp), 20)
  expect_equal(min(df_interp$Buoyant_density), BD_min)
  expect_equal(max(df_interp$Buoyant_density), BD_max)


  df_interp = lin_interp(df, n=8, BD_min=BD_min, BD_max=BD_max)
  expect_is(df_interp, 'data.frame')
  expect_equal(nrow(df_interp), 8)

  df[1,'Count'] = NA
  df_interp = lin_interp(df, n=20, BD_min=BD_min, BD_max=BD_max)
  expect_true(df_interp$Count_interp %>% is.numeric() %>% all)
})


test_that('delta BD on rep3 dataset',{
  data(physeq_rep3)
  dBD = delta_BD(physeq_rep3,
                 control_expr='Treatment=="12C-Con"')
  expect_is(dBD, 'data.frame')
  expect_equal(nrow(dBD), phyloseq::ntaxa(physeq_rep3))
})

test_that('delta BD on S2D2 list: each in parallel',{
  skip_on_cran()

  data(physeq_S2D2_l)

  doParallel::registerDoParallel(2)
  interp_l = plyr::llply(physeq_S2D2_l, delta_BD,
                         control_expr='Substrate=="12C-Con"',
                         .parallel=TRUE)
  expect_is(interp_l, 'list')
  nr = lapply(interp_l, nrow) %>% unlist %>% unique
  nt = lapply(physeq_S2D2_l, ntaxa) %>% unlist %>% unique
  expect_equal(nr, nt)
})
