solD_expected <- read.csv('files/sol.D.csv')
solI_expected <- read.csv('files/sol.csv')
BTd <- fBTd(year = 2023)
lat <- 37.2

test_that("declination in certain days", {
    expect_equal(declination(BTd), solD_expected$decl)
})

test_that("eccentricity in certain days", {
    expect_equal(eccentricity(BTd), solD_expected$eo)
})

test_that("equation of time in certain days",{
    expect_equal(eot(BTd), solD_expected$EoT)
})

test_that("sunrise angle in certain days",{
    expect_equal(sunrise(BTd, lat), solD_expected$ws)
})

test_that("extraterrestrial irradiation in certain days",{
    expect_equal(bo0d(BTd, lat), solD_expected$Bo0d)
})

test_that("sun hour angle throughout the day",{
    expect_equal(sunHour(BTd), solI_expected$w)
})

test_that("zenith angle throughout the day",{
    expect_equal(zenith(BTd, lat), solI_expected$cosThzS)
})

test_that("azimuth angle throughout the day",{
    expect_equal(azimuth(BTd, lat), solI_expected$AzS)
})
