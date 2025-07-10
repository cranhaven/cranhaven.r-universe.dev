context("nmadt.hsroc")
test_that("nmadt.hsroc is working and the results are reproducible", {

  kangdata<-read.csv(file=system.file("extdata","kangdata.csv",package="NMADiagT"),
                     header=TRUE, sep=",")
  expect_error(nadt.hsroc(nstu=10, K=2, data=kangdata, testname=c("D-dimer","Ultrasonography")))
  expect_error(nadt.hsroc(nstu=12, K=1, data=kangdata, testname=c("D-dimer","Ultrasonography")))
  expect_error(nadt.hsroc(nstu=12, K=1, data=kangdata, testname=c("D-dimer")))
  skip("skipped because it takes more than 5 minutes and similar mechanism tested in nmadt.hierarchical()")
  set.seed(9)
  kang.out.hsroc <- nmadt.hsroc(nstu=12, K=2, data=kangdata, testname=c("D-dimer","Ultrasonography"))

  testname=c("D-dimer","Ultrasonography")
  K = 2
  mean = c(0.8548,0.9435)
  sd<- c(0.0767,0.0667)
  median <- c(0.8591,0.9594)
  cil<-c(0.6878,0.8059)
  ciu<-c(0.9872,1.000)
  Se.stat <- array(paste(mean, " (", sd, ")", sep = ""),dim = c(K, 1))
  colnames(Se.stat) <- "Mean (SD)"
  rownames(Se.stat) <- testname
  Se.quan <- array(paste(median, " (", cil, ", ", ciu, ")", sep = ""),dim = c(K, 1))
  colnames(Se.quan) <- "Median (95% CI)"
  rownames(Se.quan) <- testname
  Se <- list(Mean_SD = noquote(Se.stat), Median_CI = noquote(Se.quan))

  expect_equal(as.numeric(unlist(kang.out.hsroc$Se)),as.numeric(unlist(Se)) + 0.06)
})
