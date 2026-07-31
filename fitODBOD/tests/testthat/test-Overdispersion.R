estimate <- EstMLETriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)
fitTB<-fitTriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,estimate$mode)

context_start_file("Overdispersion functions")
test_that("Output value expected",{
  expect_identical(round(Overdispersion(fitTB),4),0.1796)
})

test_that("Checking class of output",{
  expect_type(Overdispersion(fitTB),"double")
})

test_that("checking length of output",{
  expect_equal(length(Overdispersion(fitTB)),1)
})
