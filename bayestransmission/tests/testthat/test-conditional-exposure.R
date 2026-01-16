test_that("Exposure flags function works", {
  # Skip if function doesn't exist (not yet built)
  if (!exists("getExposureFlags")) {
    skip("getExposureFlags function not available - need to rebuild package")
  }
  
  flags <- getExposureFlags()
  
  expect_type(flags, "list")
  expect_true("comprehensive_testing" %in% names(flags))
  expect_true("all_classes" %in% names(flags))
  expect_true("minimal" %in% names(flags))
  
  # Should have exactly one TRUE
  flag_values <- unlist(flags)
  expect_equal(sum(flag_values), 1)
})

test_that("Critical classes are always available", {
  # These should always be available regardless of build configuration
  expect_true(exists("CppSystem"))
  expect_true(exists("CppSystemHistory")) 
  expect_true(exists("CppModel"))
  
  # Test that they can be instantiated
  expect_s4_class(CppSystem, "C++Class")
})

test_that("Conditional class availability", {
  # Skip if function doesn't exist (not yet built)
  if (!exists("getExposureFlags")) {
    skip("getExposureFlags function not available - need to rebuild package")
  }
  
  flags <- getExposureFlags()
  
  if (flags$comprehensive_testing || flags$all_classes) {
    # These should be available in comprehensive testing builds
    expect_true(exists("CppEvent"))
    expect_true(exists("CppFacility"))
    expect_true(exists("CppPatient"))
    expect_true(exists("CppSampler"))
    expect_true(exists("CppUnit"))
  } else {
    # In minimal builds, these might not be available
    # Don't test for their absence as they might still be there
    # Just record what we have
    testthat::skip("Conditional classes - availability depends on build configuration")
  }
  
  if (flags$all_classes) {
    # These should only be in full builds
    expected_optional <- c("CppAbxLocationState", "CppCountLocationState", 
                          "CppHistoryLink", "CppLocationState")
    for (cls in expected_optional) {
      if (exists(cls)) {
        expect_s4_class(get(cls), "C++Class", info = paste("Class", cls))
      }
    }
  }
})

test_that("System and SystemHistory work regardless of build", {
  # Basic functionality should work in any build configuration
  sys <- CppSystem$new(
    as.integer(simulated.data$facility),
    as.integer(simulated.data$unit),
    simulated.data$time,
    as.integer(simulated.data$patient),
    as.integer(simulated.data$type)
  )
  
  expect_s4_class(sys, "Rcpp_CppSystem")
  expect_equal(sys$startTime(), 0)
  expect_gt(sys$endTime(), 0)
})