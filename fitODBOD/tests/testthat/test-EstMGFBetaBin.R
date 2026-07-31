context_start_file("Checking outputs EstMGFBetaBin")
test_that("NA values are avoided",{
  expect_error(EstMGFBetaBin(0:7,c(47,54,43,40,40,41,NA,95)),
               "NA or Infinite or NAN values in the Input")
})
test_that("estimate parameter a",{
  expect_identical(round(EstMGFBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)$a,6),
                   6.167982)
})
Temp<-EstMGFBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)
test_that("Checking class of output",{
  expect_s3_class(Temp,"mgf")
})
test_that("outputs",{
  expect_type(Temp$a,"double")
})
