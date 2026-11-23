#context("PreparingTheData")

df<-data.frame(cbind(c(1,2,3),c(0.5,0.2,0.3),c(0.2,0.1,0.6),c(0.1,0.1,0.8),c(0.3,0.3,0.4)))
esp=rbind(c(0.5,0.2,0.3),c(0.2,0.1,0.6),c(0.1,0.1,0.8),c(0.3,0.3,0.4))

df2<-data.frame(cbind(c(1,2,3,4,5),c(0.1,0.1,0.1,0.2,0.5),c(0.2,0.2,0.2,0.2,0.2),c(0.2,0.3,0.1,0.2,0.2)))
esp2=rbind(c(0.1,0.1,0.1,0.2,0.5),c(0.2,0.2,0.2,0.2,0.2),c(0.2,0.3,0.1,0.2,0.2))


test_that("PreparingTheData", {
  expect_equal(PreparingTheData(df,Pred=0), list( E=4, Tt=3, especieOriginal=esp, especiemodiOriginal=esp[,-1]))
  expect_equal(PreparingTheData(df2,Pred=0), list( E=3, Tt=5, especieOriginal=esp2, especiemodiOriginal=esp2[,-1]))
  expect_equal(PreparingTheData(df2,Pred=4), list( E=3, Tt=3, especieOriginal=esp2[,-c(4,5)], especiemodiOriginal=esp2[,-c(1,4,5)], K=5, especieOriginal.All=esp2, especiemodiOriginal.All=esp2[,-1]))

})
