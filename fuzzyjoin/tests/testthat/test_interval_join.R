context("interval_join")

x1 <- data.frame(id1 = 1:3, start = c(1, 5, 10), end = c(3, 7, 15))
y1 <- data.frame(id2 = 1:3, start = c(2, 4, 16), end = c(4, 8, 20))
b1 <- c("start", "end")

x2 <- data.frame(id1 = 1:3, start1 = c(1, 5, 10), end1 = c(3, 7, 15))
y2 <- data.frame(id2 = 1:3, start2 = c(2, 4, 16), end2 = c(4, 8, 20))
b2 <- c(start1 = "start2", end1 = "end2")

test_that("Can inner join on intervals", {
  skip_if_not_installed("IRanges")

  j <- interval_inner_join(x1, y1, by = b1)
  expect_equal(nrow(j), 2)
  expect_equal(colnames(j), c("id1", "start.x", "end.x", "id2", "start.y", "end.y"))
  expect_equal(j$id1, 1:2)
  expect_equal(j$id2, 1:2)

  j2 <- interval_inner_join(x2, y2, by = b2)
  expect_equal(nrow(j2), 2)
  expect_equal(colnames(j2), c("id1", "start1", "end1", "id2", "start2", "end2"))
  expect_equal(j2$id1, 1:2)
  expect_equal(j2$id2, 1:2)
})

test_that("Can do non-inner joins on intervals", {
  skip_if_not_installed("IRanges")

  j_left <- interval_left_join(x1, y1, by = b1)
  expect_equal(nrow(j_left), 3)
  expect_equal(j_left$start.x, x1$start)
  expect_equal(j_left$start.y, c(2, 4, NA))

  j_right <- interval_right_join(x1, y1, by = b1)
  expect_equal(nrow(j_right), 3)
  expect_equal(j_right$start.y, y1$start)
  expect_equal(j_right$start.x, c(1, 5, NA))

  j_full <- interval_full_join(x1, y1, by = b1)
  expect_equal(nrow(j_full), 4)
  expect_equal(j_full$start.x, c(x1$start, NA))
  expect_equal(j_full$start.y, c(2, 4, NA, 16))

  j_semi <- interval_semi_join(x1, y1, by = b1)
  expect_equal(j_semi, x1[1:2, ])

  j_anti <- interval_anti_join(x1, y1, by = b1)
  expect_equal(j_anti, x1[3, ])
})

test_that("Can do inner joins on intervals with findOverlaps arguments", {
  skip_if_not_installed("IRanges")

  j_maxgap <- interval_inner_join(x1, y1, maxgap = 1.1)
  expect_equal(j_maxgap$id1, c(1, 1, 2, 2, 3, 3))
  expect_equal(j_maxgap$id2, c(1, 2, 1, 2, 2, 3))
})

test_that("Can join integer and double columns", {
  skip_if_not_installed("IRanges")

  x1_int <- x1
  x1_int$start <- as.integer(x1_int$start)
  x1_int$end <- as.integer(x1_int$end)

  j_int <- interval_inner_join(x1, y1, by = b1)

  expect_equal(j_int$start.x, head(x1_int$start, 2))
  expect_equal(j_int$end.x, head(x1_int$end, 2))
})

x3 <- data.frame(id1 = 1:3,
                 start = as.Date(c("2016-07-01", "2017-01-01", "2010-01-01")),
                 end =   as.Date(c("2016-07-15", "2017-01-10", "2011-01-01")))

x4 <- data.frame(id2 = 1:3,
                 start = as.Date(c("2016-07-10", "2017-01-09", "2012-01-01")),
                 end =   as.Date(c("2016-07-18", "2017-01-11", "2013-01-01")))

# datetimes
x5 <- x3
x6 <- x4

x5$start <- as.POSIXct(x5$start)
x5$end <- as.POSIXct(x5$end)
x6$start <- as.POSIXct(x6$start)
x6$end <- as.POSIXct(x6$end)

test_that("Can do inner joins on dates and datetimes", {
  skip_if_not_installed("IRanges")

  j_date <- interval_inner_join(x3, x4, by = b1)
  expect_equal(nrow(j_date), 2)
  expect_equal(j_date$id1, 1:2)
  expect_equal(j_date$id2, 1:2)
  expect_equal(j_date$start.x, head(x3$start, 2))
  expect_equal(j_date$start.y, head(x4$start, 2))

  j_date_anti <- interval_anti_join(x3, x4, by = b1)
  expect_equal(nrow(j_date_anti), 1)

  # with minoverlap
  j_overlap <- interval_inner_join(x3, x4, minoverlap = 3, by = b1)
  expect_equal(nrow(j_overlap), 1)
  expect_equal(j_overlap$id1, 1)
  expect_equal(j_overlap$id2, 1)

  j_date2 <- interval_inner_join(x5, x6, by = b1)
  expect_equal(nrow(j_date2), 2)
  expect_equal(j_date2$id1, 1:2)
  expect_equal(j_date2$id2, 1:2)
  expect_equal(j_date2$start.x, head(x5$start, 2))
  expect_equal(j_date2$start.y, head(x6$start, 2))
})

test_that("Joining non-compatible formats throws an error", {
  skip_if_not_installed("IRanges")

  expect_error(interval_inner_join(x1, x3, by = b1), "Cannot join")

  expect_error(interval_inner_join(x4, x6, by = b1), "Cannot join")
})

