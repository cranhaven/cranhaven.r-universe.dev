context("gather.biclustermd")

test_that("test that gather() returns all entries in the data provided to biclustermd()", {
  sbc <- biclustermd(synthetic)
  gsbc <- gather(sbc)
  
  expect_equal(nrow(gsbc), prod(dim(sbc$data)))
})

test_that("test that gather() retains all row names", {
  sbc <- biclustermd(synthetic)
  gsbc <- gather(sbc)
  
  expect_equal(sort(unique(gsbc$row_name)), sort(rownames(sbc$data)))
})

test_that("test that gather() retains all column names", {
  sbc <- biclustermd(synthetic)
  gsbc <- gather(sbc)
  
  expect_equal(sort(unique(gsbc$col_name)), sort(colnames(sbc$data)))
})

test_that("test that gather()$value matches the sparsity of the provided data", {
  sbc <- biclustermd(synthetic)
  gsbc <- gather(sbc)
  
  expect_equal(mean(is.na(gsbc$value)), mean(is.na(sbc$data)))
})

test_that("test that gather() contains the correct number of biclusters", {
  sbc <- biclustermd(synthetic)
  gsbc <- gather(sbc)
  
  expect_equal(max(gsbc$bicluster_no), prod(dim(sbc$A)))
})
