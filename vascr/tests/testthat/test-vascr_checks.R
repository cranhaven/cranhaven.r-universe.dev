test_that("Check duplicate", {
  
  map_4 = tribble(~SampleID, ~Row, ~Column, ~Sample,
                  1, "A", "1 2 3", "10 nM Treatment 1 + 1nm water",
                  2, "B", "1 2 3", "100 nM Treatment 1 + 1nm water",
                  3, "A", "1 2 3", "10 nM Treatment 2 + 1nm water", # Repeat line
                  4, "D", "1 2 3", "100 nM Treatment 2 + 1nm water")
  
  expect_snapshot(vascr_check_duplicate(map_4, "Row"))
  
  map_5 = tribble(~SampleID, ~Row, ~Column, ~Sample,
                  1, "A", "1 2 3", "10 nM Treatment 1 + 1nm water",
                  2, "B", "1 2 3", "100 nM Treatment 1 + 1nm water",
                  3, "C", "1 2 3", "10 nM Treatment 1 + 1nm water", # Repeat line
                  4, "D", "1 2 3", "100 nM Treatment 2 + 1nm water")
  
  expect_snapshot(vascr_check_duplicate(map_5, "Sample"))
  
  expect_snapshot(vascr_check_duplicate(map_5, "Row")) # Check nothing happens if there isn't a repeat
  
})

test_that("Check col exists", {
  
  map_4 = tribble(~SampleID, ~Row, ~Column, ~Sample,
                  1, "A", "1 2 3", "10 nM Treatment 1 + 1nm water",
                  2, "B", "1 2 3", "100 nM Treatment 1 + 1nm water",
                  3, "A", "1 2 3", "10 nM Treatment 2 + 1nm water", # Repeat line
                  4, "D", "1 2 3", "100 nM Treatment 2 + 1nm water")
  
  expect_snapshot(vascr_check_col_exists(map_4, "Row")) # No output, row exists
  expect_snapshot(vascr_check_col_exists(map_4, "Not_A_Col")) # Output, row issue
  
  
})


test_that("Checking of resampling works", {
  
  expect_snapshot(suppressMessages({
  w16 = system.file('extdata/instruments/ecis_16_testplate.abp', package = 'vascr')
  d16 = vascr_import("ECIS", raw = w16, experiment = "W16")
  }))
  
  expect_snapshot(vascr_check_resampled(d16))
})


test_that("Replicate consistency checks work", {
  
  expect_snapshot(vascr_check_replicates(growth.df))
  
   d1 = growth.df %>% vascr_subset(sampleid = 3, experiment = c(1,2))
   d2 = growth.df %>% vascr_subset(sampleid = c(1,2))
   data.df = rbind(d1,d2)
  
   
   expect_snapshot(vascr_check_replicates(data.df))
})

