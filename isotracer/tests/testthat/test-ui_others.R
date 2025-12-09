### * delta2prop()

test_that("delta2prop() works as expected", {
  deltas <- c(78, 5180, 263, 1065, NA, 153, 345)
  prop15N <- delta2prop(deltas, "d15N")
  prop13C <- delta2prop(deltas, "d13C")
  expect_equal(prop15N,
               c(0.00394762152189379, 0.0222160052542983, 0.00462195781097235, 
                 0.00753476874290997, NA, 0.00422111119066776, 0.0049205608555297))
  expect_equal(prop13C,
               c(0.0119085180639525, 0.0646271547716549, 0.0139237321677228, 
                 0.022565731721466, NA, 0.0127264886885013, 0.0148143353577914))
  prop15N_manual <- delta2prop(deltas, 0.0036765)
  prop13C_manual <- delta2prop(deltas, 0.011180)
  expect_equal(prop15N, prop15N_manual)
  expect_equal(prop13C, prop13C_manual)
  expect_message(delta2prop(), "Available standards are:")
  expect_message(delta2prop(), "d18O.SMOW")
  expect_error(delta2prop(deltas, c("a", "b")), "must be of length 1")
  expect_error(delta2prop(deltas, deltas), "must be of length 1")
  expect_error(delta2prop(deltas, deltas), "maybe you forgot the quotes")
  expect_error(delta2prop(deltas, "toto"), "unknown")
})
