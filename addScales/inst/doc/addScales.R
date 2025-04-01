## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE,
                      cache.path = "inst/path",
                      fig.dim = c(8,7),
                      fig.align = "center")

## ----yearly summaries,echo = TRUE, cache = TRUE, fig.dim = c(5,5)-------------
library(addScales)
data(NYCTemps) ## load the data sets
## Extract Month and Year from DATE COLUMNS
NYC <-within(NYCTemps, {
              Daily <- .5*(TMAX + TMIN)
              Month <- factor(months(as.Date(DATE)), levels = month.name)
              Year <- as.numeric(substring(DATE,1,4))
})
## Summarize by median yearly temperature
yearly <- aggregate(Daily ~ Year, data = NYC, FUN = median)
## Plot, overlaying a smooth 'loess' trend curve
trellis.par.set(plot.symbol = list(col = "darkblue"),
               plot.line = list(col = "darkblue"))
xyplot(Daily ~ Year, data = yearly, 
       type = c("l","g"),
       col = "darkblue",
       panel = function(x,y,...){
           panel.xyplot(x,y,...)
           panel.loess(x,y, col = "maroon", lwd = 1.5,
                       span = .6, deg = 2)
       },
       ylab = "Median Yearly Temp",
       main = "NYC Yearly Median Temperatures Over Time"
)

## ----Same scales, cache = TRUE------------------------------------------------
monthly <- aggregate(Daily ~  Year + Month, data = NYC, FUN = mean)  
nyctemps <-
   xyplot(Daily ~ Year|Month, type = "l", layout = c(3,4),
          data = monthly,
          as.table = TRUE,
          between = list(x=1, y=0),
          ## reduce strip size
          par.strip.text = list(lines = .8, cex = .7),
          ## remove blank space for top axis
          par.settings = list(layout.heights = list(axis.top = 0)),
          panel = function(...){
             panel.grid(v = -1, h = 0, col = "gray70")
             panel.xyplot(...)
             panel.loess(..., span = .5, col = "darkred",
             lwd = 1.5)
          },
          scales = list(axs = "i", alternating = 1, tck = c(1,0)),
          xlab = "Year",
          ylab = "Average Temperature (\u00B0F)",
          main = "Mean Monthly Historical Temperatures in NYC"
   )
nyctemps

## ----Free scales, cache = TRUE, cache.rebuild = TRUE, echo = TRUE-------------
nyctemps <- update(nyctemps,
                   prepanel = function(trim.x, trim.y,...)
                       prepanel.trim(trim.x = 0, trim.y = .05,...),
                   scales = list(axs = "i", alternating = 1, tck = c(1,0),
                                 y = list(relation = "free"))
)
nyctemps

## ----addsc temp, cache = TRUE,  cache.rebuild = TRUE, echo = TRUE-------------
nyctemps <- update(nyctemps, scales = list(axs = "i",
                                           alternating = 1, tck = c(1,0),
                                 y = list(relation = "free", draw = FALSE))
)
addScales(nyctemps)

## ---- Temp Region, cache = TRUE-----------------------------------------------
addScales(nyctemps, scaleType = "region")

## ---- more colCode, cache = TRUE, echo = TRUE---------------------------------
addScales(nyctemps, scaleType = "region", colCode = "m")

## ---- Temp colCode, cache = TRUE, echo = TRUE---------------------------------
addScales(nyctemps, scaleType = "region", colCode = "r")

