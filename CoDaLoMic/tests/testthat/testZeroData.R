set.seed(2)
dat=gtools::rdirichlet(6,c(1,2,3,1,2,3))
dat2=dat
dat2[2,1]=0
dat2[2,2]=dat[2,1]+dat[2,2]
dat2[4,3]=0
dat2[4,4]=dat[4,3]+dat[4,4]

X <- cbind( c(1:6) ,dat2)

Final=ZeroData(X,"multKM",1)
Final2=ZeroData(X,"multRepl",1)

test_that("ZeroData", {
  expect_equal(any(Final==0),FALSE,tolerance=1e-5)
  expect_equal(any(Final2==0),FALSE,tolerance=1e-5)
})


