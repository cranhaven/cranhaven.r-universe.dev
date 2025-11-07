BTd <- fBTd(year = 2023)
lat <- 37.2
solD <- fSolD(37.2, BTd)
solD_expected <- read.csv('files/sol.D.csv')

test_that('declination in fSolD',{
    expect_equal(solD$decl, solD_expected$decl)
})

test_that('eccentricity in fSolD',{
    expect_equal(solD$eo, solD_expected$eo)
})

test_that('equation of time in fSolD',{
    expect_equal(solD$EoT, solD_expected$EoT)
})

test_that('solar time in fSolD',{
    expect_equal(solD$ws, solD_expected$ws)
})

test_that('extraterrestrial irradiance in fSolD',{
    expect_equal(solD$Bo0d, solD_expected$Bo0d)
})
