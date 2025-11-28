library(pipenostics)

d     <- c(812.8, 219.0)  # [mm]
wth   <- c( 19.1,  14.5)  # [mm]
uts   <- c(530.9, 455.1)  # [N/mm^2]
l     <- c(203.2, 200.0)  # [mm]
depth <- c( 13.4,   9.0)  # [mm]

test_that("*dnvpf* errs in failure pressure", {
  expect_equal(
    dnvpf(d, wth, uts, depth, l), c(15.8663, 34.0118),
    tolerance = 1e-4
  )
})

test_that("*pcorrcpf* errs in failure pressure", {
  expect_equal(
    pcorrcpf(d, wth, uts, depth, l), c(16.3545, 33.0129),
    tolerance = 1e-4
  )
})

test_that("*shell92pf* errs in failure pressure", {
  expect_equal(
    shell92pf(d, wth, uts, depth, l), c(11.0926, 25.2729),
    tolerance = 1e-4
  )
})

data(b31gdata)
test_that("*b31gmodpf* errs in failure pressure", {
  with(b31gdata[-(6:7), ],
    expect_equal(
      b31gmodpf(d, wth, smys, depth, l),
      c(1498.8988, 452.7847, 1599.8124, 934.4549, 1076.3933, 1638.7173,
        1500.1792, 1497.9821, 1495.3120, 1496.6265),
      tolerance = 1e-4
    )
  )
})

test_that("*b31gpf* gives wrong results", {
  with(b31gdata[-(6:7), ],
       expect_equal(
         b31gpf(d, wth, smys, depth, l),
         c(1526.4724, 566.4437, 1439.5333, 629.2, 958.1, 1690.7166, 1581.4999,
           1579.8054, 1577.7257, 1578.7589),
         tolerance = 1e-4
       )
  )
})


test_that("*strderate* gives specified minimum yield of stress", {
  data(api5l3t)
  expect_equal(
    strderate(mpa_psi(api5l3t[["uts"]]),seq(0, 250, length.out = nrow(api5l3t))),
    c(310.2641, 330.9483, 413.6854, 398.6854, 404.3697, 415.0540, 439.5278,
      457.1068, 460.8963, 485.3701, 530.5282),
    tolerance = 1e-5
  )
})
