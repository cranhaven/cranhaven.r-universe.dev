

test_that("Can import and ECIS file", {
  raw = system.file('extdata/instruments/ecis_TimeResample.abp', package = 'vascr')
  modeled = system.file('extdata/instruments/ecis_TimeResample_RbA.csv', package = 'vascr')
  
  w16 = system.file('extdata/instruments/ecis_16_testplate.abp', package = 'vascr')
  empty = system.file('extdata/instruments/ecis_resample_empty.csv', package = 'vascr')
 
  #Then run the import
  
  expect_snapshot(ecis_import(raw = w16))
  expect_snapshot(ecis_import(model = empty))
  

  expect_snapshot(ecis_import(raw ,modeled, experimentname = "TEST"))
})



test_that("Can import cellZScope file", {
  model = system.file("extdata/instruments/zscopemodel.txt", package = "vascr")
  raw = system.file("extdata/instruments/zscoperaw.txt", package = "vascr")
  
  expect_snapshot(cellzscope_import(raw, model, "test"))
  expect_snapshot(cellzscope_import(raw, model))
  
})


test_that("Can import xCELLigence file", {

  testthat::skip_on_ci()
  testthat::skip_on_cran()
  
# xCELLigence test
  rawdata = system.file('extdata/instruments/xcell.plt', package = 'vascr')
  
  expect_snapshot(import_xcelligence(rawdata = rawdata,"TEST7"))
  expect_snapshot(suppressMessages(import_xcelligence(rawdata = rawdata)))
  
  tempfile = paste(tempdir(),"/","TEMPMDBFORIMPORT.mdb", sep = "")
  file.copy(from = rawdata, to = tempfile)
  expect_snapshot_error(import_xcelligence(rawdata = rawdata))
  unlink(tempfile)
  
})

