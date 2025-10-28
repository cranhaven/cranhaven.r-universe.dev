test_that("Input errors", {
  data0=matrix(runif(100*5),100,5)
  colnames(data0)=c("y",paste("x",1:3,sep=""),"t")
  data0[1,1]=NA
  formula=y~x1+x2+x3
  expect_error(Batched(formula,data0,time="t",S=10),"The dataset have missing data")

  data1=matrix(runif(100*5),100,5)
  colnames(data1)=c("y",paste("x",1:3,sep=""),"t")
  formula1=y~x1+x2+x3+x4
  expect_error(Batched(formula1,data1,time="t",S=10),"There exist undefined variables in formula")

  formula2=y~x1+x2+x3
  expect_error(Batched(formula2,data1,time="t",S=10.5),"S need to be a positive integer")

  data2=matrix(runif(100*5),100,5)
  colnames(data2)=c("y",paste("x",1:3,sep=""),"t")
  data2[which(data2[,5]>0.5),5]=0.9
  expect_error(Batched(formula2,data2,time="t",S=10),"There exist at least one interval with no data")

  data3=matrix(runif(100*5),100,5)
  colnames(data3)=c("y",paste("x",1:3,sep=""),"t")
  data.batched=Batched(formula2,data3,time="t",S=10)
  expect_error(DLSSM.init(data.batched, S0=11, vary.effects=c("x1","x2","x3")),"Number of batches of training dataset exceed S")
  })
