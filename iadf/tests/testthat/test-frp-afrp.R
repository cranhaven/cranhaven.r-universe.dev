context("(adjusted) false ring proportion")

n <- 10 #even number
dat <-  within(data.frame(row.names = seq.int(1601, length.out = n)), {
  x = c(rep(1, n/2 + 1), rep(0, n/2 - 3), rep(NA, 2)); y = x[n:1]
})

out <- c(1, 1, 0.5, 0.5, 1, 1, 0.5, 0.5, 1, 1)
names(out) <- seq.int(1601, length.out = n)



#testdatensets
df_na <- within(data.frame(row.names = seq.int(1601, length.out = n)), {
  x = rep(NA, n); y = rep(NA, n)
})


#frp
test_that("frp returns right values and handles NA correctly", {
  expect_that(frp(dat), equals(data.frame(year = as.numeric(names(out)),
                                          frp = out, stringsAsFactors = FALSE)))
  expect_that(frp(df_na),
              gives_warning("there are no trees in year: 1601, 1602, 1603, 1604, 1605, 1606, 1607, 1608, 1609, 1610"))
})

test_that("frp handles wrong input classes", {
  expect_that(frp(list(1, 2)),
              throws_error("x has to be a data.frame or matrix"))
  expect_that(frp(c(1, 2)),
              throws_error("x has to be a data.frame or matrix"))
  expect_that(frp(c("a", "b")),
              throws_error("x has to be a data.frame or matrix"))
  expect_that(frp(factor(1, 2)),
              throws_error("x has to be a data.frame or matrix"))
})



#afrp
out <- c(1, 1, 0.5 * sqrt(2), 0.5 * sqrt(2), 1 * sqrt(2), 1 * sqrt(2),
         0.5 * sqrt(2), 0.5 * sqrt(2), 1, 1)
names(out) <- seq.int(1601, length.out = n)
test_that("afrp returns right values and handles NA correctly", {
  expect_that(afrp(dat),
              equals(data.frame(year = as.numeric(names(out)),
                                afrp = out, stringsAsFactors = FALSE)))
  expect_that(afrp(df_na),
              gives_warning("there are no trees in year: 1601, 1602, 1603, 1604, 1605, 1606, 1607, 1608, 1609, 1610"))
})

test_that("afrp handles wrong input classes", {
  expect_that(afrp(list(1, 2)),
              throws_error("x has to be a data.frame or matrix"))
  expect_that(afrp(c(1, 2)),
              throws_error("x has to be a data.frame or matrix"))
  expect_that(afrp(c("a", "b")),
              throws_error("x has to be a data.frame or matrix"))
  expect_that(afrp(factor(1, 2)),
              throws_error("x has to be a data.frame or matrix"))
})
