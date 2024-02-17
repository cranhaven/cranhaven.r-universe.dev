context("Testing conservation score module")
# rtracklayer::import get error on Windows: UCSC library operation failed
skip_on_cran()

test_that("Test get_conservation_scores will output a string of phastCons scores of input coordinates", {
  variants <- read_csv("testdata/variants_sample.csv")
  scores <- get_conservation_scores(variants, file.path(system.file("extdata", "Conservation_scores", package = "utr.annotation"), "hg38.phastCons100way.bw"))
  expect_true(length(scores) == 13)
  expect_equal(scores, c("0.002;0.58;0.894", "1", "0.008", "1", "1", "1;1;1;0.999;0.999", "0.042", "0.003", "0", "0.002", "1", "1", "1"))

})

test_that("Test get_conservation_scores will output a string of phyloP scores of input coordinates", {
  variants <- read_csv("testdata/variants_sample.csv")
  scores <- get_conservation_scores(variants, file.path(system.file("extdata", "Conservation_scores", package = "utr.annotation"), "hg38.phyloP100way.bw"))
  expect_true(length(scores) == 13)
  expect_equal(scores, c("-0.894;0.787;1.239", "6.806", "0.641", "2.649", "1.849", "2.48;2.607;4.156;0.496;0.724", "0.421", "0.016", "-0.412", "1.055", "9.999", "3.369", "6.916"))

})

test_that("Test get_conservation_scores will output NA if a postion doesn't have the conservation score", {
  variants <- data.frame(Chr = c("chr1", "chr1"), Pos=c(100, 10916), Ref=c("A", "CC"), stringsAsFactors = F)
  scores <- get_conservation_scores(variants, file.path(system.file("extdata", "Conservation_scores", package = "utr.annotation"), "hg38.phastCons100way.bw"))
  expect_true(length(scores) == 2)
  expect_equal(scores, c("NA", "NA;NA"))
})

