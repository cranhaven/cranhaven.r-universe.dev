
test_that("function 'get_vdelta' works", {
  depoF <- test_rfield3logit()
  
  # Single change
  k <- sample(seq_along(depoF$vdelta), 1)
  depoC <- get_vdelta(rownames(depoF$B)[k], depoF)
  expect_identical(depoC, versor(k, length(depoF$vdelta)))
  
  # Composite change
  k <- sample(seq_along(depoF$vdelta), 2)
  depoC <- get_vdelta(
    paste(rownames(depoF$B)[k[1]], '-', rownames(depoF$B)[k[2]]),
    depoF
  )
  depoN <- versor(k[1], length(depoF$vdelta)) - versor(k[2], length(depoF$vdelta))
  expect_identical(depoC, depoN)
})



test_that("factor delimiters syntax works", {
  # Fit the model
  modVote <- nnet::multinom(
    formula = vote ~ educ + gender + race + birthyr,
    data = droplevels(USvote2016),
    trace = FALSE
  )
  p0 <- list(rep(1 / 3, 3))
  
  
  # Single factor
  depo <- field3logit(modVote, '<<gender>>', p0 = p0, nstreams =  1, narrows = 1)
  expect_is(depo + NULL, 'multifield3logit')
  expect_identical(length(depo + NULL), 1L)
  
  depo <- field3logit(modVote, '<<race>>', p0 = p0, nstreams =  1, narrows = 1)
  expect_is(depo, 'multifield3logit')
  expect_identical(length(depo), 5L)
  
  
  # Two factors
  depo <- field3logit(modVote, '<<gender>> + <<race>>', p0 = p0, nstreams =  1, narrows = 1)
  expect_is(depo, 'multifield3logit')
  expect_identical(length(depo), 5L)
  
  depo <- field3logit(modVote, '<<race>> + <<race>>', p0 = p0, nstreams =  1, narrows = 1)
  expect_is(depo, 'multifield3logit')
  expect_identical(length(depo), 25L)
  
  depo <- field3logit(modVote, '<<educ>> + <<race>>', p0 = p0, nstreams =  1, narrows = 1)
  expect_is(depo, 'multifield3logit')
  expect_identical(length(depo), 25L)
  
  depo <- field3logit(modVote, '<<race>> + <<birthyr>>', p0 = p0, nstreams =  1, narrows = 1)
  expect_is(depo, 'multifield3logit')
  expect_identical(length(depo), 25L)
  
  depo <- field3logit(modVote, '<<educ>> + <<birthyr>>', p0 = p0, nstreams =  1, narrows = 1)
  expect_is(depo, 'multifield3logit')
  expect_identical(length(depo), 25L)
  
  
  # Nested effects
  list(
    list(delta = 'genderFemale', label = 'Female'),
    list(delta = '<<race>>', label = 'Race')
  ) %>%
    field3logit(modVote, ., p0 = p0, nstreams =  1, narrows = 1) -> depo
    
  expect_is(depo, 'multifield3logit')
  expect_identical(length(depo), 6L)
})



test_that("function 'namnum2expr' works", {
  expect_identical(namnum2expr(c(x = 32, y = 10)), '32 * x + 10 * y')
  expect_identical(namnum2expr(c(x = -2, y = 10)), '-2 * x + 10 * y')
  expect_identical(namnum2expr(c(x = 32, y = -1)), '32 * x - y')
  expect_identical(namnum2expr(c(x = 1, y = 1)), 'x + y')
  expect_identical(namnum2expr(c(x = 1, y = 1, Z1 = -0.1)), 'x + y - 0.1 * Z1')
  
  depo <- namnum2expr(c(x = 1, y = 1, Z1 = -0.1, Z2 = 0.3))
  expect_identical(depo, 'x + y - 0.1 * Z1 + 0.3 * Z2')
  depo <- namnum2expr(c(x.w = 1, yq3 = 1, Z1 = -0.1, Z2 = 0.3))
  expect_identical(depo, '`x.w` + yq3 - 0.1 * Z1 + 0.3 * Z2')
  depo <- namnum2expr(c(x = 1, y = 1, Z1 = -0.1, Z2 = 0.3, Hag = 0))
  expect_identical(depo, 'x + y - 0.1 * Z1 + 0.3 * Z2')
  depo <- namnum2expr(c(Hag = 0, x = 1, y = 1, Z1 = -0.1, Z2 = 0.3))
  expect_identical(depo, 'x + y - 0.1 * Z1 + 0.3 * Z2')
  depo <- namnum2expr(c(x = 1e-12, y = 1.34e9, Z1 = -0.1, Z2 = 0.3))
  expect_identical(depo, '1e-12 * x + 1.34e+09 * y - 0.1 * Z1 + 0.3 * Z2')
  
  expect_identical(namnum2expr(c(.x = 1, y = 1)), '`.x` + y')
  expect_identical(namnum2expr(c(`3x` = 1, y = 1)), '`3x` + y')
  expect_identical(namnum2expr(c(`x:wk` = 1, y = 1)), '`x:wk` + y')
  expect_identical(namnum2expr(c(`poly(x, 2)[1]` = -3, y = 1)), '-3 * `poly(x, 2)[1]` + y')
  expect_identical(namnum2expr(c(H3 = 1, y = 1)), 'H3 + y')
  
  expect_error(namnum2expr(c(3, y = 1)))
})



test_that("labels of multifield3logit object are properly set", {
  modVote <- nnet::multinom(vote ~ educ + gender + race + birthyr,
    data = droplevels(USvote2016), trace = FALSE)
  
  depo <- field3logit(modVote, '<<race>>', label = 'Race ')
  expect_identical(labels(depo), paste('Race', levels(USvote2016$race)[-1]))
  
  depo <- field3logit(modVote, '<<race>>', label = '')
  expect_identical(labels(depo), levels(USvote2016$race)[-1])
  
  depo <- field3logit(modVote, c('raceBlack', 'raceAsian'), c('BLACK', 'ASIAN'))
  expect_identical(labels(depo), c('BLACK', 'ASIAN'))
  
  depo <- field3logit(modVote, '<<race>>', label = LETTERS[seq_len(nlevels(USvote2016$race) - 1)])
  depo2 <- paste0(LETTERS[seq_len(nlevels(USvote2016$race) - 1)], levels(USvote2016$race)[-1])
  expect_identical(labels(depo), depo2)
})

