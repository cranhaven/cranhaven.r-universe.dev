lat <- 37.2;
G0dm <- c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562,
2814, 2179)
Ta <- c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2,
17.2, 15.2)
prom <- list(G0dm = G0dm, Ta = Ta, year = 2023)

module <- list(Vocn = 57.6,
               Iscn = 4.7,
               Vmn = 46.08,
               Imn = 4.35,
               Ncs = 96,
               Ncp = 1,
               CoefVT = 0.0023,
               TONC = 47)
generator <- list(Nms = 12, Nmp = 11)
inverter <- list(Ki = c(0.01, 0.025, 0.05),
                 Pinv = 25000,
                 Vmin = 420,
                 Vmax = 750,
                 Gumb = 20)

prod <- prodGCPV(lat = lat, dataRad = prom,
                 keep.night = FALSE,
                 module = module,
                 inverter = inverter,
                 generator = generator)
prodI <- as.data.tableI(prod)
prodD <- as.data.tableD(prod)
prodM <- as.data.tableM(prod)
prodY <- as.data.tableY(prod)
prodI_expected <- read.csv('files/prodFixed.csv', sep = ';')
prodD_expected <- read.csv('files/prodFixed.D.csv', sep = ';')
prodM_expected <- read.csv('files/prodFixed.M.csv', sep = ';')
prodY_expected <- read.csv('files/prodFixed.Y.csv', sep = ';')

test_that('Performance of a fixed grid connected PV system', {
    expect_equal(prodI$Pac, prodI_expected$Pac)
    expect_equal(prodI$Pdc, prodI_expected$Pdc)
    expect_equal(prodD$Eac, prodD_expected$Eac)
    expect_equal(prodD$Edc, prodD_expected$Edc)
    expect_equal(prodD$Yf, prodD_expected$Yf)
    expect_equal(prodM$Eac, prodM_expected$Eac)
    expect_equal(prodM$Edc, prodM_expected$Edc)
    expect_equal(prodM$Yf, prodM_expected$Yf)
    expect_equal(prodY$Eac, prodY_expected$Eac)
    expect_equal(prodY$Edc, prodY_expected$Edc)
    expect_equal(prodY$Yf, prodY_expected$Yf)
})

prod2x <- prodGCPV(lat = lat, dataRad = prom,
                   modeTrk = 'two',
                   keep.night = FALSE,
                   module = module,
                   generator = generator,
                   inverter = inverter)
prodI <- as.data.tableI(prod2x)
prodD <- as.data.tableD(prod2x)
prodM <- as.data.tableM(prod2x)
prodY <- as.data.tableY(prod2x)
prodI_expected <- read.csv('files/prod2x.csv', sep = ';')
prodD_expected <- read.csv('files/prod2x.D.csv', sep = ';')
prodM_expected <- read.csv('files/prod2x.M.csv', sep = ';')
prodY_expected <- read.csv('files/prod2x.Y.csv', sep = ';')

test_that('Performance of a two tracked grid connected PV system', {
    expect_equal(prodI$Pac, prodI_expected$Pac)
    expect_equal(prodI$Pdc, prodI_expected$Pdc)
    expect_equal(prodD$Eac, prodD_expected$Eac)
    expect_equal(prodD$Edc, prodD_expected$Edc)
    expect_equal(prodD$Yf, prodD_expected$Yf)
    expect_equal(prodM$Eac, prodM_expected$Eac)
    expect_equal(prodM$Edc, prodM_expected$Edc)
    expect_equal(prodM$Yf, prodM_expected$Yf)
    expect_equal(prodY$Eac, prodY_expected$Eac)
    expect_equal(prodY$Edc, prodY_expected$Edc)
    expect_equal(prodY$Yf, prodY_expected$Yf)
})

prodHoriz <- prodGCPV(lat = lat,dataRad = prom,
                      modeTrk = 'horiz',
                      keep.night = FALSE,
                      module = module,
                      generator = generator,
                      inverter = inverter)
prodI <- as.data.tableI(prodHoriz)
prodD <- as.data.tableD(prodHoriz)
prodM <- as.data.tableM(prodHoriz)
prodY <- as.data.tableY(prodHoriz)
prodI_expected <- read.csv('files/prodHoriz.csv', sep = ';')
prodD_expected <- read.csv('files/prodHoriz.D.csv', sep = ';')
prodM_expected <- read.csv('files/prodHoriz.M.csv', sep = ';')
prodY_expected <- read.csv('files/prodHoriz.Y.csv', sep = ';')

test_that('Performance of a horizontal tracked grid connected PV system', {
    expect_equal(prodI$Pac, prodI_expected$Pac)
    expect_equal(prodI$Pdc, prodI_expected$Pdc)
    expect_equal(prodD$Eac, prodD_expected$Eac)
    expect_equal(prodD$Edc, prodD_expected$Edc)
    expect_equal(prodD$Yf, prodD_expected$Yf)
    expect_equal(prodM$Eac, prodM_expected$Eac)
    expect_equal(prodM$Edc, prodM_expected$Edc)
    expect_equal(prodM$Yf, prodM_expected$Yf)
    expect_equal(prodY$Eac, prodY_expected$Eac)
    expect_equal(prodY$Edc, prodY_expected$Edc)
    expect_equal(prodY$Yf, prodY_expected$Yf)
})
