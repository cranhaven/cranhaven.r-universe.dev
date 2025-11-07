lat <- 37.2
G0dm <- c(2.766,3.491,4.494,5.912,6.989,7.742,
         7.919,7.027,5.369,3.562,2.814,2.179)*1000
prom <- readG0dm(G0dm, lat = lat, year = 2023)
sol <- calcSol(lat, fBTd(year = 2023))
compD <- fCompD(sol, G0d = prom, corr = 'Page')
compD_expected <- read.csv('files/compD.csv', header = T, sep = ';')

test_that('daily components of radiation',{
    expect_equal(compD$Fd, compD_expected$Fd)
    expect_equal(compD$Kt, compD_expected$Kt)
    expect_equal(compD$G0d, compD_expected$G0d)
    expect_equal(compD$D0d, compD_expected$D0d)
    expect_equal(compD$B0d, compD_expected$B0d)
})
