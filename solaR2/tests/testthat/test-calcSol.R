lat <- 37.2
BTd <- fBTd(year = 2023)
sol <- calcSol(lat, BTd)
solI <- sol@solI
solD <- sol@solD
solI_expected <- read.csv('files/sol.csv')
solD_expected <- read.csv('files/sol.D.csv')

test_that('intradaily solar angles michalsky',{
    expect_equal(solI$w, solI_expected$w)
    expect_equal(solI$night, !(solI_expected$aman))
    expect_equal(solI$cosThzS, solI_expected$cosThzS)
    expect_equal(solI$AlS, solI_expected$AlS)
    expect_equal(solI$AzS, solI_expected$AzS)
    expect_equal(solI$Bo0, solI_expected$Bo0)
})

test_that('daily solar angles michalsky', {
    expect_equal(solD$decl, solD_expected$decl)
    expect_equal(solD$eo, solD_expected$eo)
    expect_equal(solD$EoT, solD_expected$EoT)
    expect_equal(solD$ws, solD_expected$ws)
    expect_equal(solD$Bo0d, solD_expected$Bo0d)
})

sol <- calcSol(lat, BTd, method = 'cooper')
solI <- sol@solI
solD <- sol@solD
solI_expected <- read.csv('files/sol_cooper.csv')
solD_expected <- read.csv('files/sol_cooper.D.csv')

test_that('sol cooper',{
    expect_equal(solI$w, solI_expected$w)
    expect_equal(solI$night, !(solI_expected$aman))
    expect_equal(solI$cosThzS, solI_expected$cosThzS)
    expect_equal(solI$AlS, solI_expected$AlS)
    expect_equal(solI$AzS, solI_expected$AzS)
    expect_equal(solI$Bo0, solI_expected$Bo0)
    expect_equal(solD$decl, solD_expected$decl)
    expect_equal(solD$eo, solD_expected$eo)
    expect_equal(solD$EoT, solD_expected$EoT)
    expect_equal(solD$ws, solD_expected$ws)
    expect_equal(solD$Bo0d, solD_expected$Bo0d)
})

sol <- calcSol(lat, BTd, method = 'spencer')
solI <- sol@solI
solD <- sol@solD
solI_expected <- read.csv('files/sol_spencer.csv')
solD_expected <- read.csv('files/sol_spencer.D.csv')

test_that('sol spencer',{
    expect_equal(solI$w, solI_expected$w)
    expect_equal(solI$night, !(solI_expected$aman))
    expect_equal(solI$cosThzS, solI_expected$cosThzS)
    expect_equal(solI$AlS, solI_expected$AlS)
    expect_equal(solI$AzS, solI_expected$AzS)
    expect_equal(solI$Bo0, solI_expected$Bo0)
    expect_equal(solD$decl, solD_expected$decl)
    expect_equal(solD$eo, solD_expected$eo)
    expect_equal(solD$EoT, solD_expected$EoT)
    expect_equal(solD$ws, solD_expected$ws)
    expect_equal(solD$Bo0d, solD_expected$Bo0d)
})

sol <- calcSol(lat, BTd, method = 'strous')
solI <- sol@solI
solD <- sol@solD
solI_expected <- read.csv('files/sol_strous.csv')
solD_expected <- read.csv('files/sol_strous.D.csv')

test_that('sol strous',{
    expect_equal(solI$w, solI_expected$w)
    expect_equal(solI$night, !(solI_expected$aman))
    expect_equal(solI$cosThzS, solI_expected$cosThzS)
    expect_equal(solI$AlS, solI_expected$AlS)
    expect_equal(solI$AzS, solI_expected$AzS)
    expect_equal(solI$Bo0, solI_expected$Bo0)
    expect_equal(solD$decl, solD_expected$decl)
    expect_equal(solD$eo, solD_expected$eo)
    expect_equal(solD$EoT, solD_expected$EoT)
    expect_equal(solD$ws, solD_expected$ws)
    expect_equal(solD$Bo0d, solD_expected$Bo0d)
})
