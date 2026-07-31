estimate <- EstMGFBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)
fitBB<-fitBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,estimate$a,estimate$b)

context("Checking residuals")
test_that("checking value 1",{
          expect_identical(round(residuals(fitBB)[1]),
                           -2)
                           })

test_that("checking value 2",{
          expect_identical(round(residuals(fitBB)[2]),
                           6)
                           })

test_that("checking class",{
          expect_that(residuals(fitBB),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(fitted(fitBB)),4)
          })
