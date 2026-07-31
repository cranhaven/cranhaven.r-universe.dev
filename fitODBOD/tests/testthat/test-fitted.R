estimate <- EstMGFBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)
fitBB<-fitBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,estimate$a,estimate$b)
context_start_file("Checking Outputs fitted")
test_that("checking value 1",{
  expect_identical(round(fitted(fitBB))[1],34)
})
test_that("checking class",{
  expect_type(fitted(fitBB),"double")
})
test_that("checking length of output",{
  expect_equal(length(fitted(fitBB)),4)
})
