test_that("Find varous things", {
  
  standard = small_growth.df %>% vascr_subset(unit = "R", frequency = 4000, time = c(5,100))
  normal = suppressMessages(vascr_normalise(standard, 100))
  
  expect_snapshot(vascr_find_normalised(standard))
  expect_snapshot(vascr_find_normalised(normal))
  
  
})

test_that("Force median", {
  
expect_snapshot(vascr:::vascr_force_median(c(1,3,5,6), "up"))
expect_snapshot(vascr:::vascr_force_median(c(1,3,5,6), "down"))


})


test_that("Vascr match", {
  
  vector = vascr_find_unit(small_growth.df, "all")
  
  expect_snapshot(vascr_match("Re", vector))
  expect_snapshot(vascr_match("Rb", vector))
  expect_snapshot(vascr_match(c("Rb", "Cm"), vector))
  
})

test_that("vascr_find_single_time", {

  expect_snapshot(vascr_find_single_time(small_growth.df, NULL)) # Null input error check
  expect_snapshot(vascr_find_single_time(small_growth.df, c(1,3))) # Two time points error check
  expect_snapshot(vascr_find_single_time(c(1,2,3), c(1,3))) # No input Df error check
  
  expect_snapshot(vascr_find_single_time(small_growth.df, c(4.876))) # Correct a made up time
  

})

test_that("vascr_find_well", {

  expect_snapshot(vascr_find_well(small_growth.df, NULL))
  expect_snapshot(vascr_find_well(small_growth.df, "A01"))
  expect_snapshot(vascr_find_well(small_growth.df, "A1"))
  expect_snapshot(vascr_find_well(small_growth.df, "W39"))

})

test_that("vascr_find_time", {

  expect_snapshot(vascr_find_time(small_growth.df, NULL))
  expect_snapshot(vascr_find_time(small_growth.df, list(1,3,5)))
  expect_snapshot(vascr_find_time(small_growth.df, Inf))
  expect_snapshot(vascr_find_time(small_growth.df, c(10,20)))
  expect_snapshot(vascr_find_time(small_growth.df, 5))
  expect_snapshot(vascr_find_time(small_growth.df, NA))
  
  expect_error(vascr_find_time(1,5))
  
})


test_that("vascr_find_frequency", {

  expect_snapshot(vascr_find_frequency(small_growth.df, 4382))
  expect_snapshot(vascr_find_frequency(small_growth.df, 4000))
  expect_snapshot(vascr_find_frequency(small_growth.df, NULL))
  expect_snapshot(vascr_find_frequency(small_growth.df, NA))
  expect_snapshot(vascr_find_frequency(small_growth.df, Inf))
  
  expect_snapshot(vascr_find_frequency(small_growth.df, "raw"))
  expect_snapshot(vascr_find_frequency(small_growth.df, "model"))
  
  expect_snapshot(suppressMessages(vascr_find_frequency(small_growth.df, frequency = c(100,200))))

})


test_that("vascr_instrument_list", {
  
  expect_snapshot(vascr_instrument_list())
  
})

test_that("vascr_units_table", {
  
  expect_snapshot(vascr_units_table())
  
})

test_that("vascr_find_instrument", {
  
  expect_snapshot(vascr_find_instrument(small_growth.df, "Rb"))
  expect_snapshot(vascr_find_instrument(small_growth.df, NULL))
  expect_snapshot(vascr_find_instrument(small_growth.df, "cellZscope"))
  expect_snapshot(vascr_find_instrument(small_growth.df, c("cellZscope", "ECIS" )))
  expect_snapshot(vascr_find_instrument(small_growth.df, c("cellZscope", "xCELLigence")))


})


test_that("vascr_find_unit", {
  
  expect_snapshot(vascr_find_unit(small_growth.df, "raw"))
  expect_snapshot(vascr_find_unit(small_growth.df, "modeled"))
  expect_snapshot(vascr_find_unit(small_growth.df, "all"))
  expect_snapshot(vascr_find_unit(small_growth.df, "Cm"))
  
  expect_snapshot(vascr_find_unit(small_growth.df, NULL))
  expect_snapshot(vascr_find_unit(small_growth.df, unit = c("Ci", "Rb")))
  
  expect_snapshot(vascr_find_unit(small_growth.df, NA))
  expect_snapshot(vascr_find_unit(small_growth.df %>% mutate(Instrument = "cellZscope"), NA))
  expect_snapshot(vascr_find_unit(small_growth.df %>% mutate(Instrument = "xCELLigence"), NA))


})

test_that("vascr_find_experiment",{

  expect_snapshot(vascr_find_experiment(small_growth.df, 1))
   expect_snapshot(vascr_find_experiment(small_growth.df, "1 : Experiment 1"))
   expect_snapshot(vascr_find_experiment(small_growth.df, NULL))

})


test_that("vascr_titles render",{
  
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  
  test_render = function(unit, frequency = 1000)
  {
    
    testdata = tribble(~x, ~y,
                       1,1,
                       2,2,
                       3,3)
    
    testgraph = ggplot()+
      geom_line(aes(x = x, y = y), data = testdata) +
      theme(axis.title.x = element_markdown(size = 30)) +
      labs(x = "TEST")
    
    graph = testgraph + labs(x = vascr_titles(unit, frequency))
    print(graph)
    expect_snapshot(vascr_titles(unit, frequency))
    vdiffr::expect_doppelganger(unit,graph)
    
  }
  
  
  test_render("C")
  test_render("R")
  test_render("P")
  test_render("X")
  test_render("Z")
  
  test_render("Rb")
  test_render("Cm")
  test_render("Alpha")
  test_render("RMSE")
  test_render("Drift")
  
  test_render("CI")
  
  
  test_render("CPE_A")
  test_render("CPE_n")
  test_render("TER")
  test_render("Ccl")
  test_render("Rmed")
  
})


test_that("vascr_titles",{
  
  
  expect_snapshot(vascr_titles("random text, not changed"))
  
  
  expect_snapshot(vascr_titles_vector(c("Rb", "R", "Cm")))

  
  
  expect_snapshot(vascr_instrument_units("ECIS"))
  expect_snapshot(vascr_instrument_units("xCELLigence"))
  expect_snapshot(vascr_instrument_units("cellZscope"))
  
  expect_snapshot(vascr_instrument_from_unit("Rb"))
  expect_snapshot(vascr_instrument_from_unit("CI"))
  expect_snapshot(vascr_instrument_from_unit("TER"))
  
})


test_that("test if data is summarised",
{
  
expect_snapshot(vascr_find_level(small_growth.df))
expect_snapshot(vascr_find_level(vascr_summarise(small_growth.df, level = "experiments")))
expect_snapshot(vascr_find_level(vascr_summarise(small_growth.df, level = "summary")))
})

test_that("test vascr file validation", {
  
  test_file_path = system.file('extdata/instruments/ecis_TimeResample.abp', package = 'vascr')
  
  # Check a file with the right extension passes
  expect_snapshot(vascr_validate_file(test_file_path, "abp"))
  # Check a file with one of two right extensions passes
  expect_snapshot(vascr_validate_file(test_file_path, extension = c("abp", "r")))

# check a file that does not exist fails
  expect_error(vascr_validate_file("non_existant_file.R", "P"))
# Check a file with the wrong extension fails
  expect_error(vascr_validate_file(test_file_path, "P"))
# Check a file with the wrong extensions fail
  expect_error(vascr_validate_file(test_file_path, c("P", "q")))



})

test_that("test well standardisation" , {
  
  expect_snapshot(vascr_standardise_wells('A01'))
  expect_snapshot(vascr_standardise_wells('A 1'))

  expect_snapshot(vascr_standardise_wells('tortoise')) # Non-standardize becomes NA
  expect_snapshot(vascr_standardise_wells(small_growth.df$Well) %>% head())

})

test_that("96 well names are correct", {
  expect_snapshot(vascr_96_well_names())
})

test_that("vascr_gg_hue",{
  expect_snapshot(vascr_gg_color_hue(5))
})

test_that("vascr_colnames_works", {
  expect_snapshot(vascr_cols())
  expect_snapshot(vascr_cols(small_growth.df, set = "exploded"))
  expect_snapshot(vascr_cols(small_growth.df, set = "core"))
  expect_snapshot(vascr_cols(small_growth.df, set = "not_a_set"))
})


test_that("Printing vascr names works", {
  expect_snapshot(vascr_samples(small_growth.df))
})


test_that("Find metadata works", {

  expect_snapshot(vascr_find_metadata(small_growth.df))

})


test_that("find col works", {

  expect_snapshot(vascr_find_col(small_growth.df, "HCMEC/D3"))
  expect_snapshot(vascr_find_col(small_growth.df, "line"))

})


test_that("SampleID from sample works", {

  expect_snapshot(vascr_find_sampleid_from_sample(small_growth.df, "5,000_cells + HCMEC D3_line"))

})

  test_that("Find sample works", {
    vascr_find_sample(small_growth.df)
     expect_snapshot(vascr_find_sample(small_growth.df, NA))
   
})

# test_that("", {
#   
#   expect_snapshot()
#   
# })





