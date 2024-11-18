test_that('Getting params for subsetting', {
  X = c('Substrate', 'Day')
  params = get_treatment_params(physeq_S2D2, X)
  expect_is(params, 'data.frame')
  expect_true(all(X %in% colnames(params)))

  X = c('Treatment')
  params = get_treatment_params(physeq_rep3, X)
  expect_is(params, 'data.frame')

  X = c('Treatment', 'Replicate')
  params = get_treatment_params(physeq_rep3, X)
  expect_is(params, 'data.frame')
})


test_that('Expression parameter extraction regardless of quotes',{
  x = "(Substrate=='12C-Con' & Day=='14') | (Substrate=='13C-Cel' & Day == '14')"
  y = '(Substrate=="12C-Con" & Day=="14") | (Substrate=="13C-Cel" & Day == "14")'
  xx = expr_param_extract(x)
  expect_true(all(c( "12C-Con", "14", "13C-Cel","14") %in% xx))
  yy = expr_param_extract(y)
  expect_true(all(c( "12C-Con", "14", "13C-Cel","14") %in% yy))
})

test_that('Expression parameter extraction regardless of expr vector length',{
  # returns a matrix
  x = c('(Substrate=="12C-Con" & Day=="14")',
        '(Substrate=="13C-Cel" & Day == "14")')
  xx = expr_param_extract(x)
  expect_is(xx, 'matrix')

  # returns a list
  y = c('(Substrate=="12C-Con" & Day=="14")',
        '(Substrate=="13C-Cel" & Day == "14")',
        '(Substrate=="13C-Cel")')
  yy = expr_param_extract(y)
  expect_is(yy, 'list')
})

test_that('Subsetting phyloseq object',{
  # params for subseting
  params = get_treatment_params(physeq_S2D2, c('Substrate', 'Day'))
  expect_is(params, 'data.frame')
  expect_equal(nrow(params), 6)
  ## filtering params
  params = dplyr::filter(params, Substrate!='12C-Con')
  expect_equal(nrow(params), 4)

  # subsetting phyloseq
  ex = "(Substrate=='12C-Con' & Day=='${Day}') | (Substrate=='${Substrate}' & Day == '${Day}')"
  physeq_S2D2_l = phyloseq_subset(physeq_S2D2, params, ex)
  expect_is(physeq_S2D2_l, 'list')
  expect_equal(length(physeq_S2D2_l), 4)
})


test_that('Subsetting phyloseq object',{
  # params for subseting
  params = get_treatment_params(physeq_rep3, c('Treatment', 'Replicate'))
  expect_is(params, 'data.frame')
  expect_equal(nrow(params), 6)
  ## filtering params
  params = dplyr::filter(params, Treatment!='12C-Con')
  expect_equal(nrow(params), 3)

  # subsetting phyloseq
  ex = "(Treatment=='12C-Con' & Replicate=='${Replicate}') | (Treatment=='${Treatment}' & Replicate=='${Replicate}')"
  physeq_rep3_l = phyloseq_subset(physeq_rep3, params, ex)
  expect_is(physeq_rep3_l, 'list')
  expect_equal(length(physeq_rep3_l), 3)
})


test_that('Naming phyloseq object',{
  # params for subseting
  params = get_treatment_params(physeq_S2D2, c('Substrate', 'Day'))
  ## filtering params
  params = dplyr::filter(params, Substrate!='12C-Con')

  # subsetting phyloseq
  ex = "(Substrate=='12C-Con' & Day=='${Day}') | (Substrate=='${Substrate}' & Day == '${Day}')"
  physeq_S2D2_l = phyloseq_subset(physeq_S2D2, params, ex)
})
