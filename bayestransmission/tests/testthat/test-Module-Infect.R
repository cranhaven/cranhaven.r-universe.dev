# Tests for classes exposed in src/Module-Infect.cpp
# Classes are tested in the order they appear in the C++ file

# Test data setup (used by multiple tests)
data(simulated.data, package = "bayestransmission")

# 1. AbxLocationState ----
test_that("CppAbxLocationState properties", {
  skip("Need to create standalone test - currently tested via integration")
})

# 2. AbxPatientState ----
test_that("CppAbxPatientState properties", {
  skip("Need to create standalone test - currently tested via integration")
})

# 3. CountLocationState ----
test_that("CppCountLocationState properties", {
  skip("Need to create standalone test - currently tested via integration")
})

# 4. Event ----
test_that("CppEvent properties and methods", {
  skip("Need to create standalone test - currently tested via integration")
})

# 5. Facility ----
test_that("CppFacility constructor and properties", {
  skip_if_not_exposed("CppFacility")
  fac <- CppFacility$new(1L)
  expect_s4_class(fac, "Rcpp_CppFacility")
  expect_equal(fac$id, 1L)
})

# 6. HistoryLink ----
test_that("CppHistoryLink properties", {
  skip("Need to create standalone test - currently tested via integration")
})

# 7. LocationState ----
test_that("CppLocationState properties", {
  skip("Need to create standalone test - currently tested via integration")
})

# 8. Model ----
test_that("CppModel base class properties", {
  skip("Abstract class - tested via derived classes in other modules")
})

# 9. Patient ----
test_that("CppPatient constructor and properties", {
  skip_if_not_exposed("CppPatient")
  pat <- CppPatient$new(42L)
  expect_s4_class(pat, "Rcpp_CppPatient")
  expect_equal(pat$id, 42L)
  
  # Test group property
  pat$group <- 5L
  expect_equal(pat$group, 5L)
})

# 10. PatientState ----
test_that("CppPatientState constructor and properties", {
  skip_if_not_exposed("CppPatientState")
  pat <- CppPatient$new(1L)
  
  # Constructor with Patient only
  ps1 <- CppPatientState$new(pat)
  expect_s4_class(ps1, "Rcpp_CppPatientState")
  
  # Constructor with Patient and nStates
  ps2 <- CppPatientState$new(pat, 3L)
  expect_s4_class(ps2, "Rcpp_CppPatientState")
})

# 11. RawEventList ----
test_that("CppRawEventList constructor and methods", {
  skip_if_not_exposed("CppRawEventList")
  expect_s4_class(CppRawEventList, "C++Class")

  rel <- CppRawEventList$new(
    rep(0, 10),         # facilities
    rep(0:1, each = 5), # units
    1:10,               # times
    rep(1, 10),         # patients
    rep(0:1, 5)         # types
  )

  expect_s4_class(rel, "Rcpp_CppRawEventList")
  expect_equal(rel$FirstTime(), 1)
  expect_equal(rel$LastTime(), 10)
})

test_that("CppRawEventList with simulated data", {
  skip_if_not_exposed("CppRawEventList")
  rel <- CppRawEventList$new(
    simulated.data$facility,
    simulated.data$unit,
    simulated.data$time,
    simulated.data$patient,
    simulated.data$type
  )
  
  expect_s4_class(rel, "Rcpp_CppRawEventList")
  expect_true(rel$FirstTime() <= rel$LastTime())
})

test_that("Empty CppRawEventList", {
  skip_if_not_exposed("CppRawEventList")
  rel <- CppRawEventList$new(
    integer(0),  # facilities
    integer(0),  # units
    numeric(0),  # times
    integer(0),  # patients
    integer(0)   # types
  )
  
  expect_s4_class(rel, "Rcpp_CppRawEventList")
  expect_equal(rel$FirstTime(), 0)
  expect_equal(rel$LastTime(),0)
})


# 12. Sampler ----
test_that("CppSampler constructor", {
  skip_if_not_exposed("CppSampler")
  # Create system and model for sampler
  sys <- CppSystem$new(
    simulated.data$facility,
    simulated.data$unit,
    simulated.data$time,
    simulated.data$patient,
    simulated.data$type
  )
  
  model <- CppLinearAbxModel$new(2, 10, 1, 0)
  hist <- CppSystemHistory$new(sys, model, FALSE)
  rr <- RRandom$new()
  
  sampler <- CppSampler$new(hist, model, rr)
  expect_s4_class(sampler, "Rcpp_CppSampler")
})

# 13. System ----
test_that("CppSystem constructor and properties", {
  sys <- CppSystem$new(
    simulated.data$facility,
    simulated.data$unit,
    simulated.data$time,
    simulated.data$patient,
    simulated.data$type
  )
  
  expect_s4_class(sys, "Rcpp_CppSystem")
  expect_equal(sys$startTime(), 0)
  expect_equal(sys$endTime(), 1734)
  # Note: log property may contain debug output depending on build
})
test_that("CppSystem with empty events", {
  sys <- CppSystem$new(
    integer(0),  # facilities
    integer(0),  # units
    numeric(0),  # times
    integer(0),  # patients
    integer(0)   # types
  )
  
  expect_s4_class(sys, "Rcpp_CppSystem")
  expect_equal(sys$startTime(), 0)
  expect_equal(sys$endTime(), 0)
})

test_that("CppSystem with minimal data", {
  sys <- CppSystem$new(
    rep(0, 5),    # facilities
    rep(0, 5),    # units
    1:5,          # times
    rep(1, 5),    # patients
    rep(0, 5)     # types
  )
  
  expect_s4_class(sys, "Rcpp_CppSystem")
  expect_equal(sys$startTime(), 1)
  expect_equal(sys$endTime(), 5)
  sys$countEvents()
})

# 14. SystemHistory ----
test_that("CppSystemHistory constructor and properties", {
  sys <- CppSystem$new(
    simulated.data$facility,
    simulated.data$unit,
    simulated.data$time,
    simulated.data$patient,
    simulated.data$type
  )
  
  model <- CppLinearAbxModel$new(2, 10, 1, 0)
  hist <- CppSystemHistory$new(sys, model, FALSE)
  
  expect_s4_class(hist, "Rcpp_CppSystemHistory")
  
  # Test properties (require ALL_CLASSES)
  skip_if_method_not_available(hist, "UnitHeads", "Requires ALL_CLASSES for Map access")
  expect_s4_class(hist$UnitHeads, "Rcpp_CppMap")
  expect_s4_class(hist$PatientHeads, "Rcpp_CppMap")
  expect_s4_class(hist$FacilityHeads, "Rcpp_CppMap")
  
  # Check that we have the expected number of units
  expect_equal(hist$UnitHeads$size, 3)
})

test_that("CppSystemHistory getEventList method", {
  skip("getEventList requires ALL_CLASSES")
  sys <- CppSystem$new(
    simulated.data$facility,
    simulated.data$unit,
    simulated.data$time,
    simulated.data$patient,
    simulated.data$type
  )
  
  model <- CppLinearAbxModel$new(2, 10, 1, 0)
  hist <- CppSystemHistory$new(sys, model, FALSE)
  
  # Get the event list
  event_list <- hist$getEventList()
  
  # Should return a list
  expect_type(event_list, "list")
  
  # Should have events (simulated data is not empty)
  expect_gt(length(event_list), 0)
  
  # First element should be an Event reference class
  expect_s4_class(event_list[[1]], "Rcpp_CppEvent")
  
  # Event should have accessible properties
  first_event <- event_list[[1]]
  expect_type(first_event$Time, "double")
  expect_type(first_event$Type, "character")
  expect_type(first_event$isTest, "logical")
  expect_type(first_event$isAdmission, "logical")
  
  # Events should be in chronological order
  if (length(event_list) > 1) {
    times <- sapply(event_list, function(e) e$Time)
    expect_true(all(diff(times) >= 0))
  }
})

test_that("CppSystemHistory getEventList with small dataset", {
  skip("getEventList requires ALL_CLASSES")
  # Create a minimal system with known events
  sys <- CppSystem$new(
    c(0L, 0L, 0L),      # facilities
    c(0L, 0L, 0L),      # units
    c(1.0, 5.0, 10.0),  # times
    c(1L, 1L, 1L),      # patients
    c(0L, 10L, 1L)      # types (admission, test, discharge)
  )
  
  model <- CppLinearAbxModel$new(2, 10, 1, 0)
  hist <- CppSystemHistory$new(sys, model, FALSE)
  
  event_list <- hist$getEventList()
  
  # Should have events
  expect_gt(length(event_list), 0)
  
  # All should be Event objects
  for (i in seq_along(event_list)) {
    expect_s4_class(event_list[[i]], "Rcpp_CppEvent")
  }
  
  # Check that we have events at the input times or close to them
  # (SystemHistory may add additional events like start/stop)
  times <- sapply(event_list, function(e) e$Time)
  
  # Should have events around the input times
  expect_true(any(times >= 0.5 & times <= 1.5))   # Around time 1
  expect_true(any(times >= 9.0 & times <= 11.0))  # Around time 10
})

test_that("CppSystemHistory getEventList event properties", {
  skip("getEventList requires ALL_CLASSES")
  sys <- CppSystem$new(
    simulated.data$facility,
    simulated.data$unit,
    simulated.data$time,
    simulated.data$patient,
    simulated.data$type
  )
  
  model <- CppLinearAbxModel$new(2, 10, 1, 0)
  hist <- CppSystemHistory$new(sys, model, FALSE)
  event_list <- hist$getEventList()
  
  # Test that we can access various event properties
  if (length(event_list) >= 10) {
    evt <- event_list[[10]]
    
    # Required properties should be accessible
    expect_no_error(evt$Time)
    expect_no_error(evt$Type)
    expect_no_error(evt$isTest)
    expect_no_error(evt$isPositiveTest)
    expect_no_error(evt$isClinicalTest)
    expect_no_error(evt$isAdmission)
    
    # Related objects should be accessible (may be NULL)
    expect_no_error(evt$Patient)
    expect_no_error(evt$Unit)
    expect_no_error(evt$Facility)
    
    # If Patient exists, should be able to access its id
    if (!is.null(evt$Patient)) {
      expect_type(evt$Patient$id, "integer")
    }
    
    # If Unit exists, should be able to access its id
    if (!is.null(evt$Unit)) {
      expect_type(evt$Unit$id, "integer")
    }
    
    # If Facility exists, should be able to access its id
    if (!is.null(evt$Facility)) {
      expect_type(evt$Facility$id, "integer")
    }
  }
})

# 15. TestParamsAbx ----
test_that("CppTestParamsAbx class exists", {
  skip_if_not_exposed("CppTestParamsAbx")
  expect_s4_class(CppTestParamsAbx, "C++Class")
  skip("Constructor parameters not documented - needs investigation")
})

# 16. Unit ----
test_that("CppUnit constructor and properties", {
  skip_if_not_exposed("CppUnit")
  fac <- CppFacility$new(1L)
  unit <- CppUnit$new(fac, 5L)
  
  expect_s4_class(unit, "Rcpp_CppUnit")
  expect_equal(unit$id, 5L)
  
  # Test getName method
  name <- unit$getName()
  expect_type(name, "character")
})
