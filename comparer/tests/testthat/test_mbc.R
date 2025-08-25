# library(testthat)

context("mbc tests")

test_that("mbc basic runs", {
  expect_error(m1 <- mbc(mean, median, input=rnorm(100)), regexp = NA)
  expect_is(m1, "mbc")
  expect_is(m1, "list")
  # expect_equal(length(m1), 5)

  # Get error when x not specified # No longer an error, actually works
  # expect_error(m1 <- mbc(mean, median, inputi={rnorm(100)}))
  # Test inputi
  expect_error(m1 <- mbc(mean(x), median(x), inputi={x=rnorm(100)}), regexp = NA)
  # Error if both input and inputi given it
  expect_error(m1 <- mbc(mean, median, input=rnorm(100), inputi={rnorm(100)}))
  # Give in names
  expect_error(m1 <- mbc(mean=mean(x), med=median(x), inputi={x=rnorm(100)}), regexp = NA)
  expect_equal(dimnames(m1$Output)[[1]], c("mean", "med"))
  # Give in only one name
  expect_error(m1 <- mbc(mean(x), med=median(x), inputi={x=rnorm(100)}), regexp = NA)
  expect_equal(dimnames(m1$Output)[[1]], c("mean(x)", "med"))
  # Check single name
  expect_error(m1 <- mbc(mean(2)), NA)
  expect_equal(dimnames(m1$Output)[[1]], c("mean(2)"))

  # Give in evaluator
  expect_error(m1 <- mbc(1, 2, evaluator={.}), regexp = NA)

  # Give in no input
  expect_error(m1 <- mbc(mean(rnorm(10)), median(rnorm(10))), regexp = NA)
  # Give in functions, no input
  expect_error(m1 <- mbc(function(x)mean(rnorm(10)), function(x)median(rnorm(10))), regexp = NA)

  # Try different times
  expect_error(m1 <- mbc(mean(x), median(x), inputi={x=rnorm(100)}, times=1), regexp = NA)
  expect_error(m1 <- mbc(mean(x), median(x), inputi={x=rnorm(100)}, times=2, target=.5), regexp = NA)
  expect_error(m1 <- mbc(mean(x), median(x), inputi={x=rnorm(100)}, times=20), regexp = NA)
  expect_error(m1 <- mbc(mean(x), median(x), inputi={x=rnorm(100)}, times=20, target=.5), regexp = NA)
  expect_true(length(capture.output(print(m1)))>1)

  # Check inputi as unnamed data
  expect_error(mbc(mean, inputi=rnorm(10)), NA)
  expect_error(mbc(mean, median, inputi={rexp(10)}), NA)
  # Check inputi as named single input no {}
  expect_error(mbc(mean(x), inputi=x <- rnorm(10)), NA)
  # Check inputi as list
  expect_error(mbc(mean, inputi=replicate(5, list(rnorm(10)))), NA)

  # Test evaluator
  expect_error(mbc(12, evaluator=function(., x) mean(.+x), input=13), NA)
  expect_error(mbc(12, evaluator=function() mean(.)), NA)

  expect_error(mbc(identity, function(x) x, inputi=12, times=3, post=12), NA)
  # This doesn't work anymore, need to put (x)
  expect_error(mbc(mean, median, input=rnorm(100), times=7, target=0))
  expect_error(mbc(mean(x), median(x), input=rnorm(100), times=7, target=0), NA)
  expect_error(mbc(mean(x), median(x), inputi=0:10, times=7, target=0), NA)

  # Test duplicate names
  expect_error(mbc(mean,mean,input=rnorm(3), times=2), NA)
  expect_error(mbc(mean,mean,mean,input=rnorm(3), times=2), NA)
  expect_error(mbc(m=mean,m=mean, m2=mean,input=rnorm(3), times=2), NA)
})

test_that("test mbc print", {
  # Basic with compare
  m1 <- mbc(mean, median, inputi=function(i)rnorm(100))
  # expect_error(print(m1), regexp = NA)
  expect_true(length(capture.output(print(m1)))>1)
})

test_that("test mbc metrics", {

  expect_error(m1 <- mbc(mean(x), median(x), inputi=function(i)rnorm(10)), NA)

  expect_error(m1 <- mbc(mean(x), median(x), inputi=function(i)rnorm(10), target=10), NA)
  # Give function for target
  expect_error(m1 <- mbc(mean(x), median(x), inputi=function(i)rnorm(10), target=function(i){i}), NA)
  expect_error(m1 <- mbc(mean(x), median(x), inputi=function(i)rnorm(10), target=list(1,2,3,4,5)), NA)

  # Test t and mis90 using lm
  x1 <- runif(10)
  x2 <- runif(10)
  y1 <- x1 * 1.2 + x2 * .43 - .76 + rnorm(10,0,.1)
  xdf <- data.frame(x1=runif(10), x2=runif(10))
  ydf <- with(xdf, x1 * 1.2 + x2 * .43 - .76 + rnorm(10,0,.1))
  # Just run, no compare of output
  expect_error(m1 <- mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2)), NA)
  expect_true(length(capture.output(print(m1)))>1)
  # Test target in
  expect_error(mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), targetin = xdf, target=ydf), NA)
  # mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), targetin = cbind(xdf, ydf), target="ydf")

  # Test t
  m1 <- mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), target=ydf, metric="t", post=function(mod){predict(mod, xdf,se=T)})
  expect_true("Mean t" %in% m1$Output_disp$Stat)
  expect_true(length(capture.output(print(m1)))>1)

  # Test mis90
  m1 <- mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), target=ydf, metric="mis90", post=function(mod){predict(mod, xdf,se=T)})
  expect_true("mis90" %in% m1$Output_disp$Stat)
  # Test giving in as function, list, and character as name from input
  expect_error(mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), target=function(i) {ydf}, metric="mis90", post=function(mod){predict(mod, xdf,se=T)}), NA)
  expect_error(mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), target=lapply(1:5, function(i)ydf), metric="mis90", post=function(mod){predict(mod, xdf,se=T)}), NA)
  expect_error(mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), target="ydf", metric="mis90", input=list(ydf=ydf, x1=xdf$x1, x2=xdf$x2, y1=y1),post=function(mod){predict(mod, xdf,se=T)}), NA)
  expect_error(mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), targetin=data.frame(ydf=ydf, x1=xdf$x1, x2=xdf$x2, y1=y1), target="ydf", metric="mis90", input=list(ydf=ydf, x1=xdf$x1, x2=xdf$x2, y1=y1)), NA)

  # t and mis90
  m1 <- mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), target=ydf, metric=c("t","mis90"), post=function(mod){predict(mod, xdf,se=T)})
  expect_true(("mis90" %in% m1$Output_disp$Stat) && "Mean t" %in% m1$Output_disp$Stat)

  m1 <- mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), target=ydf, metric=c("rmse","mis90","sr27"), post=function(mod){predict(mod, xdf,se=T)})
  expect_true(("mis90" %in% m1$Output_disp$Stat) && "rmse" %in% m1$Output_disp$Stat && "sr27" %in% m1$Output_disp$Stat)

})

test_that("mbc plot", {
  pdf(NULL) # Don't create Rplots.pdf file
  m1 <- mbc(mn={Sys.sleep(rexp(1,10));mean(x)},med= {Sys.sleep(rexp(1,20));median(x)}, inputi={x=rnorm(100)}, target=0)
  expect_error(plot(m1), NA)
})

test_that("kfold", {
  aa <- 1:10
  bb <- aa*1.8 + 10
  # Need to give in kfoldN
  expect_error(mbc(lm(y ~ x - 1), lm(y~x), inputi={x <- aa[ki];y <- bb[ki]}, kfold=TRUE))
  expect_error(mbc(lm(y ~ x - 1), lm(y~x), inputi={x <- aa[ki];y <- bb[ki]}, kfold=c(10,5)), NA)
  expect_error(mbc(lm(y ~ x - 1), lm(y~x), inputi={x <- aa[ki];y <- bb[ki]}, kfold=c(10,5), times=15, targetin = {data.frame(x=aa,y=bb)[-ki,]}, target='y'), NA)
  # expect_error(mbc(lm(y ~ x - 1), lm(y~x), inputi={x <- aa[ki];y <- bb[ki]}, kfold=5, kfoldN=10, post=predict(., data.frame(x=aa[-ki])), target=bb[-ki]), NA)
  # Check kfold=TRUE works
  expect_error(mbc(lm(y ~ x - 1), lm(y~x), inputi={x <- aa[ki];y <- bb[ki]}, kfold=c(10)), NA)
  # Check error for bad kfold
  expect_error(mbc(lm(y ~ x - 1), lm(y~x), inputi={x <- aa[ki];y <- bb[ki]}, kfold=c(10,3.3)))
  expect_error(mbc(lm(y ~ x - 1), lm(y~x), inputi={x <- aa[ki];y <- bb[ki]}, kfold=c(10,"5")))
  # Test post with expression
  expect_error(mbc(mean(x), inputi={x <- rnorm(100)[ki]}, kfold=c(100,5), post=.+max(ki)), NA)

})

test_that("non numeric", {
  # Logical
  expect_error(mbc(TRUE, times=8), NA)
  expect_error(mbc(x>.5, inputi=x <- runif(1)), NA)

  # Character
  expect_error(mbc("a"), NA)
  expect_error(mbc(letters[x], inputi=x <- sample(1:2,1), times=10), NA)
  expect_error(mbc(letters[x], letters[3-x], inputi=x <- sample(1:2,1), times=10), NA)

  # List
  expect_error(mbc(list(1,2)), NA)
})

# test_that("GauPro", {
#   # Trying to figure out better way to get input
#   m1 <- mbc(GauPro::Gaussian, GauPro::Matern52, times=2,
#       evaluator=GauPro::GauPro_kernel_model$new(X=x, Z=y, kernel=.)$predict(xp, se=T),
#       inputi={xp <- matrix(3,1,2)}, target=yp, metric='t')
#   expect_is(m1, "mbc")
# })
