#context("PlotDendogram")

names2=c("Bact1","Bact2","Bact3","Bact4","Bact5")
set.seed(314)
esp2=t(gtools::rdirichlet(n=6, c(1,1,5,1,1)))

Num2<-list(4,1,c(4,5),c(1,2))
Dem2<-list(5,2,3,c(4,5,3))


names3=c("Bact1","Bact2","Bact3","Bact4","Bact5","Bact6","Bact7")
set.seed(214)
esp3=t(gtools::rdirichlet(n=6, c(1,1,5,1,1,5,5)))

Num3<-list(6,c(6,7),2,c(2,5),c(2,5,4),c(6,7,3))
Dem3<-list(7,3,5,4,1,c(2, 5, 4, 1))

test_that("PlotDendogram", {
  expect_equal(PlotDendogram(esp2,names2)$Num    , Num2   , tolerance=1e-5)
  expect_equal(PlotDendogram(esp2,names2)$Dem    , Dem2   , tolerance=1e-5)
  expect_equal(PlotDendogram(esp3,names3)$Num    , Num3   , tolerance=1e-5)
  expect_equal(PlotDendogram(esp3,names3)$Dem    , Dem3   , tolerance=1e-5)

})
