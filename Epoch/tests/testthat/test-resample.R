
test_that("Epoch resampling works as expected", {
  set.seed(42)
  row_num <- 10
  col_num <- 100
  dummy_data <- matrix(
    rnorm(row_num * col_num),
    nrow = row_num,
    dimnames = list(paste0("Elec", seq_len(row_num)), NULL)
  )
  expected_times <- seq(0, by = 0.01, length.out = col_num)  # 100 Hz
  dummy_epoch <- Epoch(dummy_data, times = expected_times, samplingRate = 100)
  
  # Upsample from 100 Hz to 200 Hz
  upsampled_epoch <- resample(dummy_epoch, samplingRate = 200)
  expect_s4_class(upsampled_epoch, "Epoch")
  expect_equal(.samplingRate(upsampled_epoch), 200)
  expect_gt(ncol(upsampled_epoch), ncol(dummy_epoch))  # more columns than before
  expect_equal(nrow(upsampled_epoch), nrow(dummy_epoch))  # same # of electrodes
  expect_equal(rownames(upsampled_epoch), rownames(dummy_epoch))
  
  # Downsample from 100 Hz to 50 Hz
  downsampled_epoch <- resample(dummy_epoch, samplingRate = 50)
  expect_s4_class(downsampled_epoch, "Epoch")
  expect_equal(.samplingRate(downsampled_epoch), 50)
  expect_lt(ncol(downsampled_epoch), ncol(dummy_epoch))
  expect_equal(nrow(downsampled_epoch), nrow(dummy_epoch))
  
  # Column metadata warning
  dummy_colData <- data.frame(label = rep("A", col_num))
  colData(dummy_epoch) <- dummy_colData
  expect_warning(resample(dummy_epoch, samplingRate = 150), 
                 regexp = "Column metadata will be lost during resampling")
  
  # Invalid sampling rate
  expect_error(resample(dummy_epoch, samplingRate = -100), 
               regexp = "samplingRate must be a positive")
  
  # dentity resample: no change
  expect_warning(same_epoch <- resample(dummy_epoch, samplingRate = 100), 
                 regexp = "Column metadata will be lost during resampling")
  expect_equal(dim(same_epoch), dim(dummy_epoch))
  expect_equal(.samplingRate(same_epoch), .samplingRate(dummy_epoch))
})

