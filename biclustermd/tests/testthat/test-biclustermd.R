context("biclustermd")

test_that("test that the cell matrix A contains the correct number of (bi)clusters", {
  sbc <- biclustermd(synthetic, col_clusters = 4, row_clusters = 2)

  expect_equal(nrow(sbc$A), 2)
  expect_equal(ncol(sbc$A), 4)
  expect_equal(prod(dim(sbc$A)), 8)

})

test_that("test that the dimensions of partition matrices are correct", {
  sbc <- biclustermd(synthetic, col_clusters = 4, row_clusters = 2)

  expect_equal(ncol(sbc$P0), 4)
  expect_equal(ncol(sbc$P), 4)
  expect_equal(nrow(sbc$P0), ncol(synthetic))
  expect_equal(nrow(sbc$P), ncol(synthetic))

  expect_equal(ncol(sbc$Q0), 2)
  expect_equal(ncol(sbc$Q), 2)
  expect_equal(nrow(sbc$Q0), nrow(synthetic))
  expect_equal(nrow(sbc$Q), nrow(synthetic))

})

test_that("test that the dimensions of initial and final partition matrices match", {
  sbc <- biclustermd(synthetic, col_clusters = 4, row_clusters = 2)

  expect_equal(dim(sbc$P0), dim(sbc$P))
  expect_equal(dim(sbc$Q0), dim(sbc$Q))

})

test_that("test that the rows of .$Similarities matches the number of iterations", {
  sbc <- biclustermd(synthetic, col_clusters = 4, row_clusters = 2)

  expect_equal(nrow(sbc$Similarities), sbc$iteration)

})

test_that("test that the rows of .$SSE matches the number of iterations", {
  sbc <- biclustermd(synthetic, col_clusters = 4, row_clusters = 2)

  expect_equal(nrow(sbc$SSE), sbc$iteration)

})

test_that("test that each row belongs to one cluster and that all rows are accounted for", {

  sbc <- biclustermd(synthetic, col_clusters = 4, row_clusters = 2)

  expect_equal(max(rowSums(sbc$Q0)), 1)
  expect_equal(sum(colSums(sbc$Q0)), nrow(synthetic))

  expect_equal(max(rowSums(sbc$Q)), 1)
  expect_equal(sum(colSums(sbc$Q)), nrow(synthetic))

})

test_that("test that each column belongs to one cluster and that all columns are accounted for", {

  sbc <- biclustermd(synthetic, col_clusters = 4, row_clusters = 2)

  expect_equal(max(rowSums(sbc$P0)), 1)
  expect_equal(sum(colSums(sbc$P0)), ncol(synthetic))

  expect_equal(max(rowSums(sbc$P)), 1)
  expect_equal(sum(colSums(sbc$P)), ncol(synthetic))

})

test_that("test that the returned data is equal to the inputted data", {

  sbc <- biclustermd(synthetic, col_clusters = 4, row_clusters = 2)

  expect_equal(sbc$data, synthetic)

})

test_that("test that the similarity attribute matches the first measure input", {

  sbc <- biclustermd(synthetic, similarity = 'Rand')
  expect_equal(sbc$params$similarity, attr(sbc$Similarities, "used"))

  sbc <- biclustermd(synthetic, similarity = 'HA')
  expect_equal(sbc$params$similarity, attr(sbc$Similarities, "used"))

  sbc <- biclustermd(synthetic, similarity = 'Jaccard')
  expect_equal(sbc$params$similarity, attr(sbc$Similarities, "used"))

  suppressWarnings(sbc <- biclustermd(synthetic, similarity = c('Rand', 'HA', 'Jaccard')))
  expect_equal(sbc$params$similarity, attr(sbc$Similarities, "used"))

})
