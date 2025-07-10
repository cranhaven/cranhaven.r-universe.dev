context("nmadt.hierarchical")
test_that("nmadt.hierarchical is working and the results are reproducible", {

  kangdata<-read.csv(file=system.file("extdata","kangdata.csv",package="NMADiagT"),
  header=TRUE, sep=",")
  kangdata<-kangdata[c(-4,-7)]
  kangdata<-kangdata[-(17:28),]
  kangdata$sid <- c(rep(c(1:9),each=4))
  set.seed(9)
  kang.out.hierarchical <- nmadt.hierarchical(nstu=9, K=1, data=kangdata, testname=c("D-dimer"))

  testname=c("D-dimer")
  K = 1
  mean = c(0.8392,0.9281)
  sd<- c(0.0734,0.0470)
  median <- c(0.8455,0.9338)
  cil<-c(0.6737,0.8202)
  ciu<-c(0.9582,0.9972)
  Se.stat <- array(paste(mean, " (", sd, ")", sep = ""),dim = c(K, 1))
  colnames(Se.stat) <- "Mean (SD)"
  rownames(Se.stat) <- testname
  Se.quan <- array(paste(median, " (", cil, ", ", ciu, ")", sep = ""),dim = c(K, 1))
  colnames(Se.quan) <- "Median (95% CI)"
  rownames(Se.quan) <- testname
  Se <- list(Mean_SD = noquote(Se.stat), Median_CI = noquote(Se.quan))

  expect_equal(as.numeric(unlist(kang.out.hierarchical$Se)),as.numeric(unlist(Se))+0.06)
})
