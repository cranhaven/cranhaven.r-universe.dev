### R code from vignette source 'ReproducingMMNN2016.Rnw'

###################################################
### code chunk number 1: ReproducingMMNN2016.Rnw:100-102
###################################################
library(apc)
data <- data.asbestos.2013()


###################################################
### code chunk number 2: ReproducingMMNN2016.Rnw:115-116 (eval = FALSE)
###################################################
## data.asbestos.2013


###################################################
### code chunk number 3: ReproducingMMNN2016.Rnw:123-124
###################################################
apc.fit.table(data,"poisson.response")[1:4,1:6]


###################################################
### code chunk number 4: ReproducingMMNN2016.Rnw:138-143
###################################################
data.trunc  <- apc.data.list.subset(data,0,0,0,0,0,22,suppress.warning=TRUE)
fit.ac <- apc.fit.model(data.trunc,"poisson.response","AC")
forecast <- apc.forecast.ac(fit.ac)
cat("Peak forecast","\n")
print(forecast$response.forecast.per[1:6,])


###################################################
### code chunk number 5: ReproducingMMNN2016.Rnw:157-183
###################################################
v.WT2006    <- c(
2007,   1791,   1715,   1864,   
2008,   1835,   1755,   1920,
2009,   1869,   1788,   1953,
2010,   1902,   1817,   1990,
2011,   1926,   1842,   2015,
2012,   1947,   1859,   2042,
2013,   1964,   1874,   2062,
2014,   1979,   1881,   2079,
2015,   1988,   1886,   2099,
2016,   1990,   1885,   2100,
2017,   1988,   1875,   2100,
2018,   1978,   1870,   2100,
2019,   1966,   1851,   2083,
2020,   1945,   1821,   2070,
2021,   1916,   1790,   2045,
2022,   1881,   1753,   2014,
2023,   1841,   1709,   1984,
2024,   1799,   1668,   1945,
2025,   1745,   1612,   1893,
2026,   1692,   1549,   1839,
2027,   1625,   1485,   1780,
2028,   1557,   1416,   1710,
2029,   1486,   1338,   1639,
2030,   1412,   1268,   1558)
WT2006      <- matrix(data=v.WT2006, ncol=4,byrow=TRUE)                 


###################################################
### code chunk number 6: ReproducingMMNN2016.Rnw:189-211
###################################################
v.WT2010    <- c(
2011,   1942,   1866,   2022,           
2012,   1965,   1886,   2046,
2013,   1983,   1901,   2069,
2014,   1997,   1913,   2081,
2015,   2003,   1918,   2099,
2016,   2002,   1912,   2101,
2017,   2000,   1904,   2093,
2018,   1989,   1892,   2084,
2019,   1974,   1874,   2076,
2020,   1945,   1849,   2049,
2021,   1916,   1817,   2017,
2022,   1879,   1774,   1990,
2023,   1842,   1740,   1948,
2024,   1797,   1691,   1911,
2025,   1738,   1631,   1849,
2026,   1682,   1574,   1802,
2027,   1614,   1510,   1730,
2028,   1544,   1444,   1655,
2029,   1471,   1364,   1591,
2030,   1398,   1302,   1515)
WT2010      <- matrix(data=v.WT2010, ncol=4,byrow=TRUE)                 


###################################################
### code chunk number 7: ReproducingMMNN2016.Rnw:215-220
###################################################
data.trunc.2006 <- apc.data.list.subset(data,0,0,0,7,0,22,
    suppress.warning=TRUE)
fit.ac.2006     <- apc.fit.model(data.trunc.2006,
    "poisson.response","AC")
forecast.2006   <- apc.forecast.ac(fit.ac.2006)


###################################################
### code chunk number 8: ReproducingMMNN2016.Rnw:224-225
###################################################
data.sum.per <- apc.data.sums(data.trunc)$sums.per


###################################################
### code chunk number 9: ReproducingMMNN2016.Rnw:229-239
###################################################
plot(seq(1968,2013),data.sum.per,xlim=c(2002,2030),ylim=c(1400,2200),
    xlab="period",ylab="number of cases")
apc.polygon(forecast$response.forecast.per.ic,2013,TRUE,TRUE,
    col.line=1,lwd.line=3)
apc.polygon(forecast.2006$response.forecast.per.ic,2006,FALSE,
    lty.line=4,col.line=4,lwd.line=3)
apc.polygon(WT2006[,2:4],2006,FALSE,lty.line=2,col.line=2,lwd.line=3)
apc.polygon(WT2010[,2:4],2010,FALSE,lty.line=3,col.line=3,lwd.line=3)
legend("topleft",lty=c(1,4,2,3),col=c(1,4,2,3),lwd=3,
    legend=c("AC 2013","AC 2006","HSE 2006","HSE 2010"))


###################################################
### code chunk number 10: ReproducingMMNN2016.Rnw:243-253
###################################################
plot(seq(1968,2013),data.sum.per,xlim=c(2002,2030),ylim=c(1400,2200),
    xlab="period",ylab="number of cases")
apc.polygon(forecast$response.forecast.per.ic,2013,TRUE,TRUE,
    col.line=1,lwd.line=3)
apc.polygon(forecast.2006$response.forecast.per.ic,2006,FALSE,
    lty.line=4,col.line=1,lwd.line=3)
apc.polygon(WT2006[,2:4],2006,FALSE,lty.line=2,col.line=1,lwd.line=3)
apc.polygon(WT2010[,2:4],2010,FALSE,lty.line=3,col.line=1,lwd.line=3)
legend("topleft",lty=c(1,4,2,3),col=1,lwd=3,
    legend=c("AC 2013","AC 2006","HSE 2006","HSE 2010"))


