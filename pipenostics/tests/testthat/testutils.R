library(pipenostics)

test_that("*flux_loss* errs in calculation", {
  expect_equal(
    flux_loss(c(218, 1040), c(998, 1395) * 1e-3, c(2, 5)),
    c(80.70238, 275.00155),
    tolerance = 5e-6
  )
})

test_that("*loss_flux* errs in calculation", {
  expect_equal(
    loss_flux(c(80.70238, 275.00155), c(998, 1395) * 1e-3, c(2, 5)),
    c(218, 1040),
    tolerance = 5e-6
  )
})

test_that("*wth_d* errs in calculation", {
  gost30732dwth <- data.frame(
    d = c(
      25,  32,    38,  45,  57,  76,  89,
      108,  114,  133, 159, 219, 273,
      325,  377,  426, 530, 630, 720, 820, 920,
      1020, 1220, 1420
    ),
    wth = c(
      2.5, 3, 3, 3, 3, 3, 4, 4, 4, 4,
      4.5, 6, 7, 7, 7, 7, 7, 8, 8, 9,
      10  , 11, 11, 12
    )
  )

  expect_equal(
    all(wth_d(gost30732dwth[["d"]]) - gost30732dwth[["wth"]] == 0),
    TRUE
  )
})

test_that("*geodist* errs in calculation", {
  expect_equal(
    geodist(
      c(77.1539, 77.1539, 77.1539),
      c(-139.398, 120.398, -120.398)
      ,
      c(-77.1804, 77.1804, 77.1804),
      c(-139.55, 129.55, 129.55)
      ,
      6372795
    )
    ,
    c(17166029, 225883, 2332669)
    ,
    tolerance = 1e-6
  )
})

test_that("*geoarea* errs in calculation", {
  expect_equal(
    geoarea(
      lat1 = c(s28434 = 56.65, Miami   =  25.789106,  Hawaii       =   19.820680),
      lon1 = c(s28434 = 57.78, Miami   = -80.226529,  Hawaii       = -155.467989),
      lat2 = c(s28418 = 56.47, Bermuda =  32.294887,  NewZeland    =  -43.443219),
      lon2 = c(s28418 = 53.73, Bermuda = -64.781380,  NewZeland    =  170.271360),
      lat3 = c(point  = 57.00, SanJuan =  18.466319,  EasterIsland =  -27.112701),
      lon3 = c(point  = 57.00, SanJuan = -66.105743,  EasterIsland = -109.349668)
    )
    ,
    c(5170.969, 1147627.947, 28775528.873)
    ,
    tolerance = 1e-4
  )
})


test_that("*geointri* errs in calculation", {
  expect_true( geointri(lat1 = 56.47, lon1 = 53.73, lat2 = 58.02, lon2 = 56.30, lat3 = 56.65, lon3 = 57.78, lat = 57.000, lon = 57.000))
  expect_true(!geointri(lat1 = 56.47, lon1 = 53.73, lat2 = 58.02, lon2 = 56.30, lat3 = 56.65, lon3 = 57.78, lat = 57.140, lon = 57.395))
})