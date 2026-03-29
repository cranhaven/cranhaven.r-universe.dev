# test time conversion function
test_that("convert_POSIXct_to_NEONhdf5_time returns 24 character string", {
  expect_type(convert_POSIXct_to_NEONhdf5_time(Sys.time()), "character")
  expect_equal(nchar(convert_POSIXct_to_NEONhdf5_time(Sys.time())), 24)
})

test_that("site retrieval functions return vector of strings", {
  expect_type(terrestrial_core_sites(), "character")
  expect_type(terrestrial_gradient_sites(), "character")
  expect_type(water_isotope_sites(), "character")
})

test_that("correct number of sites in each site retrieval function vector", {
  expect_equal(length(terrestrial_core_sites()), 20)
  expect_equal(length(terrestrial_gradient_sites()), 27)
  expect_equal(length(water_isotope_sites()), 21)
})

test_that("site vectors contain known sites", {
  expect_true("ONAQ" %in% terrestrial_core_sites())
  expect_true("HARV" %in% terrestrial_core_sites())
  expect_true("TEAK" %in% terrestrial_gradient_sites())
  expect_true("BART" %in% terrestrial_gradient_sites())
  expect_true("ONAQ" %in% water_isotope_sites())
  expect_true("BARR" %in% water_isotope_sites())
})

test_that("site vectors have no duplicates", {
  expect_equal(length(unique(terrestrial_core_sites())),
               length(terrestrial_core_sites()))
  expect_equal(length(unique(terrestrial_gradient_sites())),
               length(terrestrial_gradient_sites()))
  expect_equal(length(unique(water_isotope_sites())),
               length(water_isotope_sites()))
})

test_that("core sites and gradient sites do not overlap", {
  core <- terrestrial_core_sites()
  grad <- terrestrial_gradient_sites()
  expect_length(intersect(core, grad), 0)
})

test_that("water isotope sites are subset of all terrestrial sites", {
  wiso <- water_isotope_sites()
  all_terr <- c(terrestrial_core_sites(), terrestrial_gradient_sites())
  expect_true(all(wiso %in% all_terr))
})
