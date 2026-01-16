test_that("Can create RawEventList with data.frame", {
  expect_s4_class(RawEventList, "C++Class")

  test <- RawEventList$new(
    rep(0, 10), # std::vector<int>,    // facilities
    rep(0:1, each = 5), # std::vector<int>,    // units
    1:10, # std::vector<double>, // times
    rep(1, 10), # std::vector<int>,    // patients
    rep(0:1, 5) # std::vector<int>     // types
  )

  expect_s4_class(test, "Rcpp_RawEventList")
  expect_equal(test$FirstTime(), 1)
  expect_equal(test$LastTime(), 10)
})
test_that("Can Create System class", {
  data(simulated.data, package = "bayestransmission")

  test <- CppTransmissionSystem$new(
    simulated.data$facility,
    simulated.data$unit,
    simulated.data$time,
    simulated.data$patient,
    simulated.data$type
  )
  expect_s4_class(test, "Rcpp_CppTransmissionSystem")
  expect_equal(test$start, 0)
  expect_equal(test$end, 1734)

  expect_equal(test$log, "")
})
test_that("RRandom", {
  RR <- RRandom$new()

  runif(1)
  seed <- .GlobalEnv$.Random.seed

  a <- RR$runif()
  b <- RR$runif()
  .GlobalEnv$.Random.seed <- seed
  c <- RR$runif()
  .GlobalEnv$.Random.seed <- seed
  d <- runif(1)

  expect_true(0 <= a && a <= 1)
  expect_false(a == b)
  expect_true(a == c)
  expect_true(a == d)

  seed <- .GlobalEnv$.Random.seed
  a <- RR$runif2(50, 100)
  b <- RR$runif2(50, 100)
  .GlobalEnv$.Random.seed <- seed
  c <- RR$runif2(50, 100)
  .GlobalEnv$.Random.seed <- seed
  d <- runif(1, 50, 100)

  expect_true(50 <= a && a <= 100)
  expect_false(a == b)
  expect_true(a == c)
  expect_true(a == d)


  seed <- .GlobalEnv$.Random.seed
  a <- RR$rexp()
  b <- RR$rexp()
  .GlobalEnv$.Random.seed <- seed
  c <- RR$rexp()
  .GlobalEnv$.Random.seed <- seed
  d <- rexp(1)
  expect_true(0 <= a)
  expect_false(a == b)
  expect_true(a == c)
  expect_true(a == d)

  seed <- .GlobalEnv$.Random.seed
  a <- RR$rexp1(100)
  b <- RR$rexp1(100)
  .GlobalEnv$.Random.seed <- seed
  c <- RR$rexp1(100)
  .GlobalEnv$.Random.seed <- seed
  d <- rexp(1, 100)
  expect_true(0 <= a)
  expect_false(a == b)
  expect_true(a == c)
  expect_true(a == d)

  seed <- .GlobalEnv$.Random.seed
  a <- RR$rgamma(1, 1)
  b <- RR$rgamma(1, 1)
  .GlobalEnv$.Random.seed <- seed
  c <- RR$rgamma(1, 1)
  .GlobalEnv$.Random.seed <- seed
  d <- rgamma(1, 1)
  expect_false(a == b)
  expect_true(a == c)
  expect_true(a == d)

  seed <- .GlobalEnv$.Random.seed
  a <- RR$rnorm()
  b <- RR$rnorm()
  .GlobalEnv$.Random.seed <- seed
  c <- RR$rnorm()
  .GlobalEnv$.Random.seed <- seed
  d <- rnorm(1)
  expect_false(a == b)
  expect_true(a == c)
  expect_true(a == d)

  seed <- .GlobalEnv$.Random.seed
  a <- RR$rpoisson(5)
  b <- RR$rpoisson(5)
  .GlobalEnv$.Random.seed <- seed
  c <- RR$rpoisson(5)
  .GlobalEnv$.Random.seed <- seed
  d <- rpois(1, 5)
  expect_false(a == b)
  expect_true(a == c)
  expect_true(a == d)
})

test_that("InsituParams", {
  IP <- CppInsituParams$new(3)

  expect_equal(IP$nParam, 3L)
  expect_length(IP$paramNames, 3L)
  expect_silent(IP$set(1, 1, 1))


  IP2 <- CppInsituParams$new(2)

  expect_equal(IP2$nParam, 2L)
  expect_length(IP2$paramNames, 2L)
})
test_that("CppTestParams", {
  TP <- CppTestParams$new(3)
  TP$values
  TP$paramNames
  expect_equal(TP$nParam, 3L)
  expect_length(TP$values, 3L)
  expect_length(TP$paramNames, 3L)
  expect_silent(IP$set(1, 1, 1))


  TP2 <- CppTestParams$new(2)

  expect_length(TP2$values, 2L)
  expect_length(TP2$paramNames, 2L)
})

test_that("CppLinearAbxModel", {
  sys <- CppSystem$new(
    simulated.data$facility,
    simulated.data$unit,
    simulated.data$time,
    simulated.data$patient,
    simulated.data$type
  )
  WhatAmI(sys$.pointer)

  model <- CppLinearAbxModel$new(2, 10, 1, 0)
  model$className

  icp <- model$InColParams
  class(icp)

  icp$timeOrigin <- (sys$end - sys$start)/2

  hist <- CppSystemHistory$new(sys, model, FALSE)

  model$logLikelihood(hist)

  h <- hist$UnitHeads
  expect_s4_class(h, "Rcpp_CppMap")

  expect_equal(h$size, 3)


  first <- asHistoryLink(h$FirstValue())
  last <- asHistoryLink(h$LastValue())
  if(F){
  first$linked
  first$hidden
  first$PatientPrev
  first$PatientNext
  first$HistoryNext
  first$HistoryPrev
  first$SystemPrev
  first$SystemNext
  first$FacilityPrev
  first$FacilityNext
  first$UnitPrev
  first$UnitNext
  first$Event
  first$PatientState
  first$UnitState
  first$FacilityState
  first$SystemState

  model$logLikelihood(last)

  last$linked
  last$hidden
  last$PatientPrev
  last$PatientNext
  last$HistoryNext
  last$HistoryPrev
  last$SystemPrev
  last$SystemNext
  last$FacilityPrev
  last$FacilityNext
  last$UnitPrev
  last$UnitNext
  last$Event
  last$Event$type
  last$PatientState
  last$UnitState
  last$FacilityState
  last$SystemState
}

  l <- last
  links <- list(first)

  expect_equal(first$Event$type, "start")

  expect_equal(model$logLikelihood_HL(l), 0)


  {
    (l <- l$UnitNext)
    format(l$.pointer) %>% gsub("<pointer: 0x(\\w+)>", "\\1", .) %>%
      grepl("2136af11970", .)
    #0x26e22f74ca0
    l$Event$type
    l$Event$time
    l$Event$isAdmission

    prev <- l$PatientPrev
    prev$Event$type
    prev$Event$time

    l$Event$patient$id
    prev$Event$patient$id

    l$Event$hash
    prev$Event$hash

    icp <- model$InColParams
    survtsp <- model$SurveillanceTestParams
    clintsp <- model$ClinicalTestParams
    isp <- model$InsituParams
    ocp <- model$OutColParams
    # abxp <- model$AbxParams

    prev <- l$UnitPrev
    prev$Event$type

    l$Event$time



    # icp$logProbGap(prev, l)
    LocationState_s <- asAbxLocationState(prev$UnitState)
    class(LocationState_s)

    t0 <- prev$Event$time
    t1 <- l$Event$time
    # LogNormalAbxICP::logProgressionGap icp$logProgressionGap(t0, t1, LocationState_s)
    icp$progressionRate
    LocationState_s$Never


    ll <- model$logLikelihood_HL(l)

    links <- c(links, l)
    if(identical(

    )) break
  }





  rr <- RRandom$new()
  mc <- CppSampler$new(hist, model, rr)


})



