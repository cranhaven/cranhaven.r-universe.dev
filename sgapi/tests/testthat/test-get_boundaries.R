test_that("A valid boundary returns a list", {
  expect_type(get_boundaries("MSOA_Dec_2011_Boundaries_Generalised_Clipped_BGC_EW_V3_2022","-1.282825,52.354169,0.206626,52.7106"), "list")
})


test_that("A valid boundary and valid areanames returns a list", {
  expect_type(get_boundaries_areaname(boundary="Local_Authority_Districts_December_2022_UK_BGC_V2",col_name_var="LAD22NM",chosen_constituency_list=c("Derbyshire Dales","Harrogate","West Northamptonshire")), "list")
})


test_that("A coordinate outside the UK returns a warning", {
  expect_warning(get_boundaries("MSOA_Dec_2011_Boundaries_Generalised_Clipped_BGC_EW_V3_2022","52.354169,-1.282825,52.7106,0.206626"))
})

test_that("A valid boundary containing a space in the title throws an error", {
  expect_error(get_boundaries_areaname("Local Authority_Districts_December_2022_UK_BGC_V2","LAD22NM",c("Derbyshire Dales","Harrogate")))
})

test_that("Invalid area name(s) returns an error", {
  expect_error(get_boundaries_areaname("Local Authority_Districts_December_2022_UK_BGC_V2","LAD22NM",c("Derbyshire","London")))
})

#test_that("An invalid boundary name returns a warning", {
#  expect_warning(get_boundaries("MSOA_Dec_2011Clipped_BGC_EW_V3_2022","-1.282825,52.354169,0.206626,52.7106"))
#})


test_that("An invalid column name returns a warning", {
  expect_warning(get_boundaries_areaname("Local_Authority_Districts_December_2022_UK_BGC_V2","L22NM",c("Derbyshire Dales","Harrogate")))
})

test_that("A valid boundary containing a space in the title throws an error", {
  expect_error(get_boundaries("MSOA Dec 2011_Boundaries_Generalised_Clipped_BGC_EW_V3_2022","52.354169,-1.282825,52.7106,0.206626"))
})