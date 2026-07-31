context_start_file("Checking outputs EstMLEAddBin")
test_that("NA values are avoided",{
  expect_error(EstMLEAddBin(NA,4),"NA or Infinite or NAN values in the Input")
})
test_that("estimate parameter p",{
  expect_identical(round(EstMLEAddBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)$p,6),0.580941)
})
test_that("Checking class of output",{
  expect_type(EstMLEAddBin(Chromosome_data$No.of.Asso,Chromosome_data$fre),"list")
})
