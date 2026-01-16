context("genome_join")

library(dplyr)

x1 <- tibble(id = 1:4,
             chromosome = c("chr1", "chr1", "chr2", "chr2"),
             start = c(100, 200, 300, 400),
             end = c(150, 250, 350, 450))

x2 <- tibble(id = 1:4,
             chromosome = c("chr1", "chr2", "chr2", "chr1"),
             start = c(140, 210, 400, 300),
             end = c(160, 240, 415, 320))

test_that("Can join genomes on chromosomes and intervals", {
  skip_if_not_installed("IRanges")

  j <- genome_inner_join(x1, x2, by = c("chromosome", "start", "end"))

  expect_equal(j$chromosome.x, j$chromosome.y)
  expect_equal(j$chromosome.x, c("chr1", "chr2"))
  expect_equal(j$id.x, c(1, 4))
  expect_equal(j$id.y, c(1, 3))
  expect_equal(colnames(j), c("id.x", "chromosome.x", "start.x", "end.x",
                              "id.y", "chromosome.y", "start.y", "end.y"))

  # if they were all the same chromosome, everything would get joined
  x3 <- x1
  x3$chromosome <- "chr1"
  x4 <- x2
  x4$chromosome <- "chr1"

  j2 <- genome_inner_join(x3, x4, by = c("chromosome", "start", "end"))

  expect_equal(nrow(j2), 4)
  expect_equal(j2$id.x, 1:4)
  expect_equal(j2$id.y, c(1, 2, 4, 3))

  # Left and right joins
  j3 <- genome_left_join(x1, x2, by = c("chromosome", "start", "end"))

  expect_equal(nrow(j3), 4)
  expect_equal(sum(is.na(j3$start.x)), 0)
  expect_equal(sum(is.na(j3$start.y)), 2)
  expect_true(all(j3$chromosome.x == j3$chromosome.y, na.rm = TRUE))

  j4 <- genome_right_join(x1, x2, by = c("chromosome", "start", "end"))

  expect_equal(nrow(j4), 4)
  expect_equal(sum(is.na(j4$start.x)), 2)
  expect_true(all(j4$chromosome.x == j4$chromosome.y, na.rm = TRUE))
})

test_that("genome_join throws an error if not given three columns", {
  skip_if_not_installed("IRanges")

  expect_error(genome_inner_join(x1, x2, by = c("start", "end")), "three columns")
})
