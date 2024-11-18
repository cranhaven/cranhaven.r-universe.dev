set_params = function(full=TRUE){
  # setting parameters for tests
  set.seed(2)
  M = 100                                  # number of species
  ming = 1.67                             # gradient minimum...
  maxg = 1.78                                # ...and maximum
  nfrac = 24                                 # number of gradient fractions
  locs = seq(ming, maxg, length=nfrac)       # gradient locations
  tol  = rep(0.005, M)                       # species tolerances
  h    = ceiling(rlnorm(M, meanlog=11))    # max abundances

  opt = rnorm(M, mean=1.7, sd=0.005)      # species optima
  params = cbind(opt=opt, tol=tol, h=h)  # put in a matrix

  opt1 = rnorm(M, mean=1.7, sd=0.005)      # species optima
  params1 = cbind(opt=opt1, tol=tol, h=h)  # put in a matrix
  opt2 = rnorm(M, mean=1.7, sd=0.005)      # species optima
  params2 = cbind(opt=opt2, tol=tol, h=h)  # put in a matrix
  opt3 = rnorm(M, mean=1.7, sd=0.005)      # species optima
  params3 = cbind(opt=opt3, tol=tol, h=h)  # put in a matrix
  opt4 = rnorm(M, mean=1.72, sd=0.008)      # species optima
  params4 = cbind(opt=opt4, tol=tol, h=h)  # put in a matrix
  opt5 = rnorm(M, mean=1.72, sd=0.008)      # species optima
  params5 = cbind(opt=opt5, tol=tol, h=h)  # put in a matrix
  opt6 = rnorm(M, mean=1.72, sd=0.008)      # species optima
  params6 = cbind(opt=opt6, tol=tol, h=h)  # put in a matrix


  param_l = list(
    '12C-Con_rep1' = params1,
    '12C-Con_rep2' = params2,
    '12C-Con_rep3' = params3,
    '13C-Glu_rep1' = params4,
    '13C-Glu_rep2' = params5,
    '13C-Glu_rep3' = params6
  )

  # return
  if(full==TRUE){
    L = list(locs=locs, params=param_l)
  } else {
    L = list(locs=locs, params=params)
  }
  return(L)
}


test_that('Gradient sim', {
  L = set_params(full=FALSE)
  df_OTU = gradient_sim(L$locs, L$params)
  expect_is(df_OTU, 'data.frame')
  expect_false(is.null(df_OTU$Buoyant_density))
})

test_that('phyloseq sim',{
  L = set_params()
  physeq = HTSSIP_sim(L$locs, L$params)
  expect_is(physeq, 'phyloseq')
  BDs = physeq %>% sample_data %>% .$Buoyant_density
  expect_false(is.null(BDs))
})

test_that('phyloseq sim w/ metadata',{
  # metadata
  meta = data.frame(
    'Gradient' = c('12C-Con_rep1', '12C-Con_rep2', '12C-Con_rep3',
                   '13C-Glu_rep1', '13C-Glu_rep2', '13C-Glu_rep3'),
    'Treatment' = c(rep('12C-Con', 3), rep('13C-Glu', 3)),
    'Replicate' = c(1:3, 1:3)
  )
  # simulation
  L = set_params()
  physeq = HTSSIP_sim(L$locs, L$params, meta=meta)
  expect_is(physeq, 'phyloseq')
  BDs = physeq %>% sample_data %>% .$Buoyant_density
  expect_false(is.null(BDs))
})




