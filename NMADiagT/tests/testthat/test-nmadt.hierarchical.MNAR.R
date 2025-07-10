context("nmadt.hierarchical.MNAR")
test_that("nmadt.hierarchical.MNAR is working and the results are reproducible", {

  kangdata<-read.csv(file=system.file("extdata","kangdata.csv",package="NMADiagT"),
                     header=TRUE, sep=",")
  expect_error(nadt.hsroc(nstu=10, K=2, data=kangdata, testname=c("D-dimer","Ultrasonography")))
  expect_error(nadt.hsroc(nstu=12, K=1, data=kangdata, testname=c("D-dimer","Ultrasonography")))
  expect_error(nadt.hsroc(nstu=12, K=1, data=kangdata, testname=c("D-dimer")))
  skip("skipped because it takes more than 5 minutes and similar mechanism tested in nmadt.hierarchical()")
  set.seed(9)
  kang.out.MNAR <- nmadt.hierarchical.MNAR(nstu=12, K=2, data=kangdata, testname=c("D-dimer","Ultrasonography")
                                           ,gamma1=c(-0.5,-0.5), gamma0=c(-0.5,-0.5))

  testname=c("D-dimer","Ultrasonography")
  K = 2
  mean = c(0.8461,0.9097)
  sd<- c(0.0735,0.0478)
  median <- c(0.8543,0.9144)
  cil<-c(0.6750,0.8008)
  ciu<-c(0.9621,0.9867)
  Se.stat <- array(paste(mean, " (", sd, ")", sep = ""),dim = c(K, 1))
  colnames(Se.stat) <- "Mean (SD)"
  rownames(Se.stat) <- testname
  Se.quan <- array(paste(median, " (", cil, ", ", ciu, ")", sep = ""),dim = c(K, 1))
  colnames(Se.quan) <- "Median (95% CI)"
  rownames(Se.quan) <- testname
  Se <- list(Mean_SD = noquote(Se.stat), Median_CI = noquote(Se.quan))

  expect_equal(as.numeric(unlist(kang.out.MNAR$Se)),as.numeric(unlist(Se)))
})
