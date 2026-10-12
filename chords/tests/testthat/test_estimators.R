library(chords)
context("testing estimators")

test_that("various estimators", {
  data(brazil)
  rds.object<- initializeRdsObject(brazil)
  
  expect_equal(length(rds.object$I.t), 303)
  rds.object2 <- Estimate.b.k(rds.object = rds.object )
  the.names <- names(rds.object2$estimates$Nk.estimates)
  
  expect_equal(length(rds.object2$estimates),10)
  

  # Integrated
  rds.object3 <- Estimate.b.k(rds.object = rds.object, type = 'integrated')
  expect_equal(length(rds.object3$estimates),10)
  expect_equal(sum(rds.object3$estimates$Nk.estimates), 313)
  expect_equal(names(rds.object3$estimates$Nk.estimates), the.names)
  # jack.control <- makeJackControl(1,1e1)
  # rds.object4 <- Estimate.b.k(rds.object = rds.object, type = 'leave-d-out', jack.control = jack.control)
  # expect_equal(length(rds.object4$estimates),10)
  # expect_equal(sum(rds.object4$estimates$Nk.estimates, na.rm=TRUE), 1330)
  
  
  expect_equal(length(thetaSmoothingNks(rds.object2)),32)
  rds.object5 <- Estimate.b.k(rds.object = rds.object2, type = 'parametric')
  expect_equal(length(rds.object5$estimates),10)
  expect_equal(sum(rds.object5$estimates$Nk.estimates<Inf), 999)
  expect_equal(names(rds.object5$estimates$Nk.estimates), the.names)
  
  rds.object6 <- Estimate.b.k(rds.object = rds.object2, type='rescaling')
  expect_equal(sum(rds.object6$estimates$Nk.estimates<Inf), 999)
  expect_equal(names(rds.object6$estimates$Nk.estimates), the.names)
})
