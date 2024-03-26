
eg1<-extract_filename("L HEPG2 P3 72HRS.csv")
exp1<-c("L HEPG2 P3 72HRS.csv","L-HEPG2-P3-72HRS","L" , "HEPG2","P3", "72HRS" )

eg2<-extract_filename("L HEPG2 P3 72HRS.csv", split=" ",end=".csv",remove="L",sep="")
exp2<-c("L HEPG2 P3 72HRS.csv","HEPG2P372HRS", "HEPG2","P3", "72HRS" )


context("extract filename")

test_that("examples are working", {
  expect_that(eg1, equals(exp1))
  expect_that(eg2, equals(exp2))
 })
