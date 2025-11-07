utils::globalVariables('variable')

##################################################################
## THEMES
##################################################################
xscale.solar <- function(...){ans <- xscale.components.default(...); ans$top = FALSE; ans}
yscale.solar <- function(...){ans <- yscale.components.default(...); ans$right = FALSE; ans}

solaR.theme <- function(pch = 19, cex = 0.7, region = rev(brewer.pal(9, 'YlOrRd')), ...) {
    theme <- custom.theme.2(pch = pch, cex = cex, region = region, ...)
    theme$strip.background$col <- 'transparent'
    theme$strip.shingle$col <- 'transparent'
    theme$strip.border$col <- 'transparent'
    theme
}

solaR.theme.2 <- function(pch = 19, cex = 0.7, region = rev(brewer.pal(9, 'YlOrRd')), ...) {
    theme <- custom.theme.2(pch = pch, cex = cex, region = region, ...)
    theme$strip.background$col <- 'lightgray'
    theme$strip.shingle$col <- 'lightgray'
    theme
}

##################################################################
## XYPLOT
##################################################################
setGeneric('xyplot')

setMethod('xyplot',
          signature = c(x = 'data.table', data = 'missing'),
          definition = function(x, data,
                                par.settings = solaR.theme.2,
                                xscale.components = xscale.solar,
                                yscale.components = yscale.solar,
                                scales = list(y = 'free'),
                                ...){
              N <- length(x)-1
              x0 <- x[, lapply(.SD, as.numeric), by = Dates]
              x0 <- melt(x0, id.vars = 'Dates')
              x0$variable <- factor(x0$variable,
                                    levels = rev(levels(factor(x0$variable))))
              xyplot(value ~ Dates | variable, x0,
                     par.settings = par.settings,
                     xscale.components = xscale.components,
                     yscale.components = yscale.components,
                     scales = scales,
                     type = 'l', layout = c(1,N),
                     ...)
          })

setMethod('xyplot',
          signature = c(x = 'formula', data = 'Meteo'),
          definition = function(x, data,
                                par.settings = solaR.theme,
                                xscale.components = xscale.solar,
                                yscale.components = yscale.solar,
                                ...){
              data0  <-  getData(data)
              xyplot(x, data0,
                     par.settings = par.settings,
                     xscale.components = xscale.components,
                     yscale.components = yscale.components,
                     strip = strip.custom(strip.levels = c(TRUE, TRUE)), ...)
          }
          )

setMethod('xyplot',
          signature = c(x = 'formula', data = 'Sol'),
          definition = function(x, data,
                                par.settings = solaR.theme,
                                xscale.components = xscale.solar,
                                yscale.components = yscale.solar,
                                ...){
              data0 <- as.data.tableI(data, complete = TRUE, day = TRUE)
              data0[, w := h2r(hms(Dates)-12)]
              xyplot(x, data0,
                     par.settings = par.settings,
                     xscale.components = xscale.components,
                     yscale.components = yscale.components,
                     strip = strip.custom(strip.levels = c(TRUE, TRUE)), ...)
          }
          )

setMethod('xyplot',
          signature = c(x = 'formula', data = 'G0'),
          definition = function(x, data,
                                par.settings = solaR.theme,
                                xscale.components = xscale.solar,
                                yscale.components = yscale.solar,
                                ...){
              data0 <- as.data.tableI(data, complete = TRUE, day = TRUE)
              xyplot(x, data0,
                     par.settings = par.settings,
                     xscale.components = xscale.components,
                     yscale.components = yscale.components,
                     strip = strip.custom(strip.levels = c(TRUE, TRUE)), ...)
          }
          )

setMethod('xyplot',
          signature = c(x = 'formula', data = 'Shade'),
          definition = function(x, data,
                                par.settings = solaR.theme,
                                xscale.components = xscale.solar,
                                yscale.components = yscale.solar,
                                ...){
              data0 <- as.data.table(data)
              xyplot(x, data0,
                     par.settings = par.settings,
                     xscale.components = xscale.components,
                     yscale.components = yscale.components,
                     strip = strip.custom(strip.levels = c(TRUE, TRUE)), ...)
          }
          )

setMethod('xyplot',
          signature = c(x = 'Meteo', data = 'missing'),
          definition = function(x, data,
                                ...){
              x0 <- getData(x)
              xyplot(x0,
                     scales = list(cex = 0.6, rot = 0, y = 'free'),
                     strip = FALSE, strip.left = TRUE,
                     par.strip.text = list(cex = 0.6),
                     ylab = '',
                     ...)
          }
          )

setMethod('xyplot',
          signature = c(x = 'G0', data = 'missing'),
          definition = function(x, data, ...){
              x0 <- as.data.tableD(x, complete = FALSE)              
              x0 <- melt(x0, id.vars = 'Dates')
              xyplot(value~Dates, x0, groups = variable,
                     par.settings = solaR.theme.2,
                     xscale.components = xscale.solar,
                     yscale.components = yscale.solar,
                     superpose = TRUE,
                     auto.key = list(space = 'right'),
                     ylab = 'Wh/m\u00b2',
                     type = 'l',
                     ...)     
          }
          )

setMethod('xyplot',
          signature = c(x = 'ProdGCPV', data = 'missing'),
          definition = function(x, data, ...){
              x0 <- as.data.tableD(x, complete = FALSE)
              xyplot(x0,
                     strip = FALSE, strip.left = TRUE,
                     ylab = '', ...)
          }
          )

setMethod('xyplot',
          signature = c(x = 'ProdPVPS', data = 'missing'),
          definition = function(x, data, ...){
              x0 <- as.data.tableD(x, complete = FALSE)
              xyplot(x0,
                     strip = FALSE, strip.left = TRUE,
                     ylab = '', ...)
          }
          )
