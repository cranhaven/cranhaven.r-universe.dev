library(pipenostics)
data(b31gdata)
test_that("*crvl* errs in pipe diagnostics", {
  examples <- with(b31gdata, b31crvl(maop, d, wth, smys, def, depth, l))
  delta <- round(unlist(b31gdata - examples[colnames(b31gdata)]), 5)
  expect_equal(
    all(is.nan(delta) | delta == 0),
    TRUE
  )

  expect_equal(
    capture.output(with(b31gdata[1, ], b31crvl(maop, d, wth, smys, def, depth, l))),
    c("", "-- Calculated data --", "Intermediate factor (A) = 1.847",
      "Design pressure = 1093 PSI; Safe pressure = 1093 PSI",
      "Pipe may be operated safely at MAOP, 910 PSI",
      "With corrosion length 7.500 inch, maximum allowed corrosion depth is 0.2490 inch; A = 1.847",
      "With corrosion depth 0.100 inch, maximum allowed corrosion length is Inf inch; A = 5.000"
    )
  )
})

test_that("*b31gacd* errs in allowable depth of the corroded area", {
  with(b31gdata,
       expect_equal(
         b31gacd(design_pressure, maop, d, wth, l),
         allowed_corrosion_depth
      )
  )
})

test_that("*b31gacl* errs in allowable length of the corroded area", {
  with(b31gdata,
       expect_equal(
         b31gacl(design_pressure, maop, d, wth, depth, l),
         allowed_corrosion_length
       )
  )
})

test_that("*b31gafr* errs in intermediate factor", {
  with(b31gdata,
       expect_equal(
         b31gafr(d, wth, l),
         A
       )
  )
})

test_that("*b31gdep* errs in design pressure", {
  with(b31gdata,
       expect_equal(
         trunc(b31gdep(d, wth, smys, def)),
         design_pressure
       )
  )
})


test_that("*b31gops* errs in operational status of pipe", {
  with(b31gdata,
       expect_equal(
         b31gops(wth, depth),
         status
       )
  )
})

test_that("*b31gsap* errs in safe maximum pressure", {
  with(b31gdata,
       expect_equal(
         b31gsap(design_pressure, d, wth, depth, l),
         safe_pressure
       )
  )
})


