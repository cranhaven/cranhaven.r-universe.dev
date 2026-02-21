

test_that("Can subset correctly", {
  
  expect_snapshot(vascr_subset(growth.df))
  
  
  # Time
  expect_snapshot(vascr_subset(growth.df, time = 40))
  expect_snapshot(vascr_subset(growth.df, time = c(40,60)))
  expect_snapshot(vascr_subset(growth.df, time = NULL))
  
  # Unit
  expect_snapshot( vascr_subset(growth.df, unit = "Rb"))
  expect_snapshot(vascr_subset(growth.df, unit = "R"))
  
  # Well
  expect_snapshot(vascr_subset(growth.df, well = "A1"))
  expect_snapshot(vascr_subset(growth.df, well = "B12"))
  expect_snapshot(vascr_subset(growth.df, well = "B20"))
  
  expect_snapshot(vascr_subset(growth.df, well = c("B2", "B03")))
  expect_snapshot(vascr_subset(growth.df, well = c("-A01", "-B3")))
  
  
  # Frequency
  expect_snapshot(vascr_subset(growth.df, frequency = 4000))
  expect_snapshot(vascr_subset(growth.df %>% mutate(Frequency = as.character(Frequency)), frequency = "4000"))
  
  # Experiment
  expect_snapshot(vascr_subset(growth.df, experiment = 1))
  expect_snapshot(vascr_subset(growth.df, experiment = 1))
  
  
  # Instrument
  expect_snapshot(vascr_subset(growth.df, instrument = "ECIS"))
  
  # Sample ID
  expect_snapshot(vascr_subset(growth.df, unit = "Rb", sampleid = c(1:3)))
  expect_snapshot(vascr_subset(growth.df, unit = "Rb", sampleid = c(8)))
  
  # Sub sample
  expect_snapshot(vascr_subset(growth.df, subsample = 100))
  
  
  # Check ignores warning when there is an issue
  expect_snapshot(vascr_subset(growth.df, unit = "Rb", sampleid = 10))
  
  # Check exclusion code works
  expect_snapshot(vascr_exclude(growth.df, c("A01", "E01")))
  expect_snapshot(vascr_exclude(growth.df, c("A01", "E01"), 1))
  
  # Replace sample
  expect_snapshot(vascr_edit_sample(growth.df, change_list = list(c("0_cells", "Cell Free"))))
  
  suppressMessages({
      to_rename = growth.df %>% vascr_subset(sample = c("0 cells","20,000 cells", "10,000 cells"))

       to_rename$Sample %>% unique()

      renamed = vascr_edit_sample(to_rename, change_list = list(c("0_cells", "Cell Free")))
  })

  expect_snapshot(renamed)
  
})