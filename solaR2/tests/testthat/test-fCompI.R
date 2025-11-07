lat <- 37.2
G0dm <- c(2.766,3.491,4.494,5.912,6.989,7.742,
          7.919,7.027,5.369,3.562,2.814,2.179)*1000
Ta <- c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9,
24.3, 18.2, 17.2, 15.2)
prom <- readG0dm(G0dm, lat = lat, year = 2023)
sol <- calcSol(lat, fBTd(year = 2023))
compD <- fCompD(sol, G0d = prom, corr = 'Page')
compI <- fCompI(sol, compD)
compI_expected <- read.csv('files/compI.csv', sep = ';')

test_that('Calculation of solar irradiance on a horizontal surface', {
    expect_equal(compI$G0, compI_expected$G0)
    expect_equal(compI$D0, compI_expected$D0)
    expect_equal(compI$B0, compI_expected$B0)
    expect_equal(compI$Fd, compI_expected$fd)
    expect_equal(compI$Kt, compI_expected$kt)
})
