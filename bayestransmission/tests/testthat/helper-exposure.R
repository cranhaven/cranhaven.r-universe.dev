# Test helper to check which classes are exposed
# This should be included in test setup

# Helper function to check if a class is available
check_class_available <- function(class_name) {
  tryCatch({
    exists(class_name) && is(get(class_name), "C++Class")
  }, error = function(e) FALSE)
}

# Get exposure flags and check what's available
get_exposure_info <- function() {
  flags <- getExposureFlags()
  
  # Check which classes are actually available
  critical_classes <- c("CppSystem", "CppSystemHistory", "CppModel")
  testing_classes <- c("CppEvent", "CppFacility", "CppPatient", "CppPatientState", 
                      "CppRawEventList", "CppSampler", "CppUnit", "CppMap")
  optional_classes <- c("CppAbxLocationState", "CppAbxPatientState", "CppCountLocationState",
                       "CppEpisode", "CppHistoryLink", "CppLocationState", "CppTestParamsAbx")
  
  list(
    flags = flags,
    available = list(
      critical = sapply(critical_classes, check_class_available),
      testing = sapply(testing_classes, check_class_available), 
      optional = sapply(optional_classes, check_class_available)
    )
  )
}

# Helper function to check if a method is available on an object
check_method_available <- function(object, method_name) {
  tryCatch({
    # Try to access the method/property
    env <- as.environment(object)
    exists(method_name, envir = env, inherits = FALSE) ||
      method_name %in% names(object$getClass()@refMethods)
  }, error = function(e) FALSE)
}

# Conditional test wrapper
skip_if_not_exposed <- function(class_name) {
  if (!check_class_available(class_name)) {
    testthat::skip(paste("Class", class_name, "not exposed in this build"))
  }
}

# Conditional method wrapper
skip_if_method_not_available <- function(object, method_name, reason = NULL) {
  if (!check_method_available(object, method_name)) {
    msg <- if (!is.null(reason)) {
      paste("Method", method_name, "not available:", reason)
    } else {
      paste("Method", method_name, "requires BAYESTRANSMISSION_ALL_CLASSES")
    }
    testthat::skip(msg)
  }
}