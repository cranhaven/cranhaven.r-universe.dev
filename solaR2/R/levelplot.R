setGeneric('levelplot')

setMethod('levelplot',
          signature = c(x = 'formula', data = 'Meteo'),
          definition = function(x, data,
                                par.settings = solaR.theme,
                                panel = panel.levelplot.raster, interpolate = TRUE,
                                xscale.components = xscale.solar,
                                yscale.components = yscale.solar,
                                ...){
              data0 <- getData(data)
              ind <- data0$Dates
              data0$day <- doy(ind)
              data0$month <- month(ind)
              data0$year <- year(ind)
              data0$w <- h2r(hms(ind)-12)
              levelplot(x, data0,
                        par.settings = par.settings,
                        xscale.components = xscale.components,
                        yscale.components = yscale.components,
                        panel = panel, interpolate = interpolate,
                        ...)
          }
          )

setMethod('levelplot',
          signature = c(x = 'formula', data = 'Sol'),
          definition = function(x, data,
                                par.settings = solaR.theme,
                                panel = panel.levelplot.raster, interpolate = TRUE,
                                xscale.components = xscale.solar,
                                yscale.components = yscale.solar,
                                ...){
              data0 <- as.data.tableI(data, complete=TRUE, day=TRUE)
              ind <- data0$Dates
              data0$day <- doy(ind)
              data0$month <- month(ind)
              data0$year <- year(ind)
              levelplot(x, data0,
                        par.settings = par.settings,
                        xscale.components = xscale.components,
                        yscale.components = yscale.components,
                        panel = panel, interpolate = interpolate,
                        ...)
          }
          )

setMethod('levelplot',
          signature = c(x = 'formula', data = 'G0'),
          definition = function(x, data,
                                par.settings = solaR.theme,
                                panel = panel.levelplot.raster, interpolate = TRUE,
                                xscale.components = xscale.solar,
                                yscale.components = yscale.solar,
                                ...){
              data0 <- as.data.tableI(data, complete=TRUE, day=TRUE)
              ind <- data0$Dates
              data0$day <- doy(ind)
              data0$month <- month(ind)
              data0$year <- year(ind)
              levelplot(x, data0, 
                        par.settings = par.settings,
                        xscale.components = xscale.components,
                        yscale.components = yscale.components,
                        panel = panel, interpolate = interpolate,
                        ...)
          }
          )
