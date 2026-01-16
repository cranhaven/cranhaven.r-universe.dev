test_that("runMCMC validates data is sorted by patient, then time", {
  data(simulated.data, package = "bayestransmission")
  modelParameters <- LinearAbxModel(nstates = 2)
  
  # Test 1: Unsorted by patient (sorted by time only)
  unsorted_data <- simulated.data[order(simulated.data$time), ]
  
  expect_error(
    runMCMC(
      data = unsorted_data,
      modelParameters = modelParameters,
      nsims = 1,
      nburn = 1,
      outputparam = FALSE,
      outputfinal = FALSE,
      verbose = FALSE
    ),
    regexp = "Data must be sorted by patient ID, then time",
    info = "Should reject data sorted by time only"
  )
  
  # Test 2: Within-patient time ordering violation
  bad_data <- simulated.data
  # Find a patient with multiple events and swap their order
  patient_103_rows <- which(bad_data$patient == 103)
  if (length(patient_103_rows) >= 2) {
    # Swap first two events for this patient
    temp <- bad_data[patient_103_rows[1], ]
    bad_data[patient_103_rows[1], ] <- bad_data[patient_103_rows[2], ]
    bad_data[patient_103_rows[2], ] <- temp
    
    expect_error(
      runMCMC(
        data = bad_data,
        modelParameters = modelParameters,
        nsims = 1,
        nburn = 1,
        outputparam = FALSE,
        outputfinal = FALSE,
        verbose = FALSE
      ),
      regexp = "Data must be sorted by patient ID, then time",
      info = "Should reject data with wrong time ordering within patient"
    )
  }
  
  # Test 3: Properly sorted data should work
  sorted_data <- simulated.data[order(simulated.data$patient, simulated.data$time), ]
  
  expect_no_error({
    results <- runMCMC(
      data = sorted_data,
      modelParameters = modelParameters,
      nsims = 1,
      nburn = 1,
      outputparam = FALSE,
      outputfinal = FALSE,
      verbose = FALSE
    )
  })
})
