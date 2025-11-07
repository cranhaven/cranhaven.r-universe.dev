BTd <- fBTd(year = 2023)
lat <- 37.2
solD <- fSolD(37.2, BTd)
solI <- fSolI(solD, sample = '1 hour')
solI_expected <- read.csv('files/sol.csv')

test_that('Sun hour angle in fSolI',{
    expect_equal(solI$w, solI_expected$w)
})

test_that('Zenith angle in fSolI',{
    expect_equal(solI$cosThzS, solI_expected$cosThzS)
})

test_that('Azimuth angle in fSolI',{
    expect_equal(solI$AzS, solI_expected$AzS)
})

test_that('Solar altitude angle in fSolI',{
    expect_equal(solI$AlS, solI_expected$AlS)
})

test_that('Extraterrestrial irradiance in fSolI',{
    expect_equal(solI$Bo0, solI_expected$Bo0)
})

test_that('Night in fSolI',{
    expect_equal(solI$night, !(solI_expected$aman))
})
