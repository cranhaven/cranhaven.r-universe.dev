context_start_file("Checking outputs EstMLETriBin")
test_that("NA values are avoided",{
  expect_error(EstMLETriBin(NA,4),
               "NA or Infinite or NAN values in the Input")
})
test_that("estimate parameter mode",{
  expect_identical(round(EstMLETriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)$mode,6),
                   0.707276)
})
test_that("minimized negative ll value",{
  expect_identical(round(EstMLETriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)$min,4),
                   440.6583)
})
Tempsy<-EstMLETriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)
test_that("Checking class of output",{
  expect_type(Tempsy,"list")
})
test_that("Checking value",{
  expect_type(Tempsy$min,"double")
})
