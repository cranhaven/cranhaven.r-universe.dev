
test_that('method "labels<-" works', {
  modVote <- nnet::multinom(vote ~ educ + gender + race + birthyr,
    data = droplevels(USvote2016), trace = FALSE)
  
  depo <- field3logit(modVote, '<<race>>')
  depo2 <- test_rstring(nlevels(USvote2016$race) - 1, 8)
  labels(depo) <- depo2
  expect_identical(labels(depo), depo2)
})
