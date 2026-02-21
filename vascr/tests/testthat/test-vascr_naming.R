
test_that("Vascr Import Works",
          {
            
            raw = system.file('extdata/instruments/ecis_TimeResample.abp', package = 'vascr')
            modeled = system.file('extdata/instruments/ecis_TimeResample_RbA.csv', package = 'vascr')
            expect_snapshot(suppressMessages(vascr_import("ECIS", raw, modeled, "ECIS_Data")))
            
            

	
           # cellZscope
	
             model = system.file("extdata/instruments/zscopemodel.txt", package = "vascr")
	
             raw = system.file("extdata/instruments/zscoperaw.txt", package = "vascr")
		
             expect_snapshot(vascr_import("cellzscope", raw, model, "cellZscope"))
             

             testthat::skip_on_ci()
             testthat::skip_on_cran()
             
		
             #' # ScioSpec

             raw = system.file("extdata/instruments/ScioSpec", package = "vascr")

             expect_snapshot(vascr_import("sciospec", raw, model, "ScioSpec"))
             
             expect_snapshot_error(vascr_import("inst", raw, model, "ScioSpec"))
             
             # xCELLigence
             
             raw = system.file('extdata/instruments/xcell.plt', package = 'vascr')
             
             # No modeling for this system
             
             expect_snapshot(vascr_import("xCELLigence", raw, experiment = "xCELLigence"))
            
          })

test_that("Create blank DF", {
      expect_snapshot(vascr_blank_df())
})


test_that("Apply map", {
  lookup = system.file('extdata/instruments/eciskey.csv', package = 'vascr')
  expect_snapshot(vascr_apply_map(data.df = growth.df, map = lookup))
   
  expect_snapshot(vascr_apply_map(growth.df %>% vascr_subset(well = c("A1")) %>% select(-Sample), lookup))
})


test_that("Vascr_map_template works", {
  expect_snapshot(vascr_map_template())
})


test_that("vascr regenerate map works", {
  expect_snapshot(vascr_regenerate_map(growth.df %>% mutate("Experiment" = 1)))
})

test_that("Samples work",
          {
            
            
            map_1 = tribble(~Row, ~Column, ~Sample,
                            "A", "1 2 3", "10 nM Treatment 1 + 1nm water",
                            "B", "1 2 3", "100 nM Treatment 1 + 1nm water",
                            "C", "4 5 6", "10 nM Treatment 2 + 1nm water",
                            "D", "1 2 3", "100 nM Treatment 2 + 1nm water")
            
            map_2 = tribble(~Well, ~Sample,
                            "A1 A2 A3", "10 nM Treatment 1 + 1nm water",
                            "B1 B2 B3", "100 nM Treatment 1 + 1nm water",
                            "C1 C2 C3", "10 nM Treatment 2 + 1nm water",
                            "C1 C2 C3", "100 nM Treatment 2 + 1nm water")
            
            map_3 = tribble(~Sample,
                            "10 nM Treatment 1 + 1nm water",
                            "100 nM Treatment 1 + 1nm water",
                            "10 nM Treatment 2 + 1nm water",
                            "100 nM Treatment 2 + 1nm water")
            
            map_4 = tribble(~SampleID, ~Row, ~Column, ~Sample,
                            1, "A", "1 2 3", "10 nM Treatment 1 + 1nm water",
                            2, "B", "1 2 3", "100 nM Treatment 1 + 1nm water",
                            3, "A", "1 2 3", "10 nM Treatment 2 + 1nm water", # Repeat line
                            4, "D", "1 2 3", "100 nM Treatment 2 + 1nm water")
            
            map_5 = tribble(~SampleID, ~Row, ~Column, ~Sample,
                            1, "A", "1 2 3", "10 nM Treatment 1 + 1nm water",
                            2, "B", "1 2 3", "100 nM Treatment 1 + 1nm water",
                            3, "C", "1 2 3", "10 nM Treatment 1 + 1nm water", # Repeat line
                            4, "D", "1 2 3", "100 nM Treatment 2 + 1nm water")
            
            map_6 = tribble(~SampleID, ~Row, ~Column, ~Sample,
                            1, "A", "1 2 3", "10 nM Treatment 1 + 1nm water",
                            2, "B", "1 2 3", "100 nM Treatment 1 + 1nm water",
                            3, "C", "4 5 6", "10 nM Treatment 2 + 1nm water",
                            4, "D", "1 2 3", "100 nM Treatment 2 + 1nm water")
            
            map_7 = tribble(~SampleID, ~Row, ~Column, ~Sample,
                            1, "A", "1 2 3", "10 nM Treatment 1 + 1nm water + 6nm Treatment 2",
                            2, "B", "1 2 3", "100 nM Treatment 1 + 1nm water",
                            3, "C", "4 5 6", "10 nM Treatment 2 + 1nm water",
                            4, "D", "1 2 3", "100 nM Treatment 2 + 1nm water")
            
            map_10 = tribble(~SampleID, ~Rows, ~Column, ~Sample,
                            1, "A", "1 2 3", "10 nM Treatment 1 + 1nm water + 6nm Treatment 2",
                            2, "B", "1 2 3", "100 nM Treatment 1 + 1nm water",
                            3, "C", "4 5 6", "10 nM Treatment 2 + 1nm water",
                            4, "D", "1 2 3", "100 nM Treatment 2 + 1nm water")
            
            expect_snapshot_error(vascr_import_map(map_10))
            
            expect_snapshot(vascr_import_map(map_1))
            expect_snapshot(vascr_import_map(map_2))
            expect_snapshot_error(vascr_import_map(map_3))
            expect_snapshot(vascr_import_map(map_4))
            expect_snapshot(vascr_import_map(map_5))
            expect_snapshot(vascr_import_map(map_6))
            expect_snapshot(vascr_import_map(map_7))
            
            map8 = tribble(~Well, ~Sample, 
                           "A01", "S",
                           "B03", "SM",
                           "C01", "S",
                           "D01", "SM",
                           "E01", "S",
                           "F01", "St")
            
            expect_snapshot(vascr_import_map(map8))
            
            
            expect_snapshot(vascr_explode(growth.df))
            
            lookup = system.file('extdata/instruments/eciskey.csv', package = 'vascr')
            
            expect_snapshot(vascr_import_map(lookup))
            
            map_8 = tribble(~SampleID, ~Row, ~Column, ~Sample,
                            1, "A", "1 2 3", "10_nM Treatment 1 + 1_nm water + 6_nm Treatment 2",
                            2, "B", "1 2 3", "100_nM Treatment 1 + 1_nm water",
                            3, "C", "4 5 6", "10_nM Treatment 2 + 1_nm water",
                            4, "D", "1 2 3", "100_nM Treatment 2 + 1_nm water")
            
            map_9 = vascr_import_map(map_8)
            
            explode = vascr_explode(map_9)
            
            expect_snapshot(vascr_implode(explode %>% select(-Sample)))
            
            expect_snapshot(vascr_implode(explode %>% select(-Sample), cols = "nM Treatment 1"))
            
            
          })


test_that("edit name works", {
  expect_snapshot(vascr_edit_name(growth.df,"HCMEC D3", "HCMEC/D3"))
})



test_that("Assign SampleID works",{
          expect_snapshot(growth.df %>% vascr_assign_sampleid())
          })



