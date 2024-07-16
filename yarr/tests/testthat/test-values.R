test_that("missing values are read", {
  filename <- tempfile()
  write("
@relation test
@attribute V0 numeric
@attribute V1 string
@attribute V2 {0,1,2}
@attribute V3 integer
@attribute V4 real
@data
1,?,1,1,?
?,1,?,?,1", filename)
  ds <- read.arff(filename)

  expect_true(is.na(ds[1,2]) &&
                is.na(ds[1,5]) &&
                is.na(ds[2,1]) &&
                is.na(ds[2,3]) &&
                is.na(ds[2,4])
              )

  ds <- read.arff(filename, stringsAsFactors = T)

  expect_true(is.na(ds[1,2]) &&
                is.na(ds[1,5]) &&
                is.na(ds[2,1]) &&
                is.na(ds[2,3]) &&
                is.na(ds[2,4])
              )

})
