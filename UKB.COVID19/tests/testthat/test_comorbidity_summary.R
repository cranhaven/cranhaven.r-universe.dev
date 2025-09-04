

test_that("Generate comorbidity summary",{
  
  comorb <- comorbidity_summary(ukb.data=covid_example("sim_ukb.tab.gz"),
                                hesin.file=covid_example("sim_hesin.txt.gz"), 
                                hesin_diag.file=covid_example("sim_hesin_diag.txt.gz"), 
                                ICD10.file=covid_example("ICD10.coding19.txt.gz"),
                                primary = FALSE,
                                Date.start = "16/03/2020")
  
  expect_s3_class( comorb, "data.frame" )
  expect_equal( ncol(comorb), 165)
  expect_equal( colnames(comorb)[1:3], c("ID", "A00-A09", "A15-A19"))
  
})
