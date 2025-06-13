## ---- warning=FALSE, message=FALSE--------------------------------------------
library(neverhpfilter)

data(GDPC1)

log_RGDP <- 100*log(GDPC1)

gdp_model <- yth_glm(log_RGDP["1960/"], h = 8, p = 4)

plot(gdp_model)

## ---- warning=FALSE, message=FALSE--------------------------------------------
gdp_filtered <- yth_filter(log_RGDP, h = 8, p = 4)

tail(gdp_filtered, 16)

class(gdp_filtered)

## ---- warning = FALSE---------------------------------------------------------
plot(log_RGDP, grid.col = "white", col = "blue", legend.loc = "topleft", main = "100 x Log of Real GDP (GDPC1)")
addPanel(yth_filter, output=c("cycle"), type="h", on=NA, col="darkred" )

## ---- warning = FALSE---------------------------------------------------------
Employment_log <- 100*log(PAYEMS["1950/"])

employment_cycle <- yth_filter(Employment_log, h = 24, p = 12, output = "cycle")

plot(employment_cycle, grid.col = "white", type = "h", up.col = "darkgreen", dn.col = "darkred", 
     main = "Log of Employment cycle")

## -----------------------------------------------------------------------------
gdp_5yr <- yth_filter(log_RGDP, h = 20, p = 4, output = c("x", "trend", "cycle"))

plot(gdp_5yr["1980/"][,1:2], grid.col = "white", legend.loc = "topleft", 
     main = "Log of Real GDP and 5-year trend", 
     panels = 'lines(gdp_5yr["1980/"][,3], type="h", on=NA)')

gdp_10yr <- yth_filter(log_RGDP, h = 40, p = 4, output = c("x", "trend", "cycle"))

plot(gdp_10yr["1980/"][,1:2], grid.col = "white", legend.loc = "topleft", 
     main = "Log of Real GDP and 10-year trend",
     panels = 'lines(gdp_10yr["1980/"][,3], type="h", on=NA)')


