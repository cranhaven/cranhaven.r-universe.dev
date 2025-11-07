#### Methods ####
### get ###
## extracts the latitude from the objects ##
setGeneric('getLat', function(object, units = 'rad')
{standardGeneric('getLat')})

## extracts the data for class Meteo ##
setGeneric('getData', function(object){standardGeneric('getData')})

## extracts the global irradiance for class Meteo ##
setGeneric('getG0', function(object){standardGeneric('getG0')})

### index ###
## extract the index of the daily data ##
setGeneric('indexD', function(object){standardGeneric('indexD')})

## extract the index of the intradaily data ##
setGeneric('indexI', function(object){standardGeneric('indexI')})

#### Methods for Sol ####
### getLat ###
setMethod('getLat',
          signature = (object = 'Sol'),
          definition = function(object, units = 'rad'){
              stopifnot(units %in% c('deg', 'rad'))
              result = switch(units,
                           rad = d2r(object@lat),
                           deg = object@lat)
              return(result)
          })

### show ###
setMethod('show',
          signature = (object = 'Sol'),
          definition = function(object){
              cat('Object of class Sol \n\n')
              cat('Latitude: ',
                  paste(round(getLat(object, 'deg'), 1), 'degrees\n\n'))
              cat('Daily values:\n')
              print(summary(object@solD))
              cat('\nIntradaily values: \n')
              print(summary(object@solI))
          })

### indexD ###
setMethod('indexD',
          signature = (object = 'Sol'),
          definition = function(object){as.POSIXct(object@solD$Dates)
          })


### indexI ###
setMethod('indexI',
          signature = (object = 'Sol'),
          definition = function(object){as.POSIXct(object@solI$Dates)
          })

#### Methods for Meteo ####
### getData ####
setMethod('getData',
          signature = (object = 'Meteo'),
          definition = function(object){
              result <- object@data
              return(result)
          })

### getLat ###
setMethod('getLat',
          signature = (object = 'Meteo'),
          definition = function(object, units = 'rad'){
              stopifnot(units %in% c('deg', 'rad'))
              result = switch(units,
                              rad = d2r(object@latm),
                              deg = object@latm)
              return(result)
          })

### show ###
setMethod('show', 'Meteo',
          function(object){
            cat('Object of class ', class(object),'\n\n')
            cat('Source of meteorological information: ')
            cat(paste(object@type, object@source, sep = '-'),'\n')
            cat('Latitude of source: ',
                paste(round(getLat(object,'deg'), 1), 'degrees\n\n'))
            cat('Meteorological Data:\n')
            print(summary(getData(object)))
          }
          )

### index ###
setMethod('indexD',
          signature = (object = 'Meteo'),
          definition = function(object){as.POSIXct(getData(object)$Dates)})

#### Methods for G0 ####

### getG0 ###
setMethod('getG0',
          signature = (object = 'Meteo'),
          definition = function(object){
              result <- getData(object)
              return(result$G0)
          })

### show ###
setMethod('show',
          signature = (object = 'G0'),
          definition = function(object){
              cat('Object of class ', class(object),'\n\n')
              cat('Source of meteorological information: ')
              cat(paste(object@type, object@source, sep = '-'),'\n\n')
              cat('Latitude of source: ',
                  paste(round(getLat(as(object, 'Meteo'),'deg'), 1),
                        'degrees\n'))
              cat('Latitude for calculations: ',
                  paste(round(getLat(as(object, 'Sol'), 'deg'),1), 'degrees\n\n'))
              cat('Monthly avarages:\n')
              G0dm <- copy(as.data.tableM(object))
              G0dm[, month := month(Dates)]
              G0dm[, year := year(Dates)]
              G0dm[, Dates := paste(month.abb[month], year, sep = '. ')]
              G0dm[, month := NULL]
              G0dm[, year := NULL]
              print(G0dm)
              cat('\nYearly values:\n')
              print(as.data.tableY(object))
          })

#### Methods for Gef ####

### show ###
setMethod('show',
          signature = (object = 'Gef'),
          definition = function(object){
              callNextMethod()
              cat('-----------------\n')
              cat('Mode of tracking: ', object@modeTrk,'\n')
              if (object@modeTrk == 'fixed'){
                  cat('    Inclination: ', object@angGen$beta, '\n')
                  cat('    Orientation: ', object@angGen$alpha, '\n')
              } else {
                  cat('    Inclination limit:', object@angGen$betaLim, '\n')
              }
          })

#### Methods for prodGCPV ####

### show ###
setMethod('show',
          signature = (object = 'ProdGCPV'),
          definition = function(object){
              callNextMethod()
              cat('-----------------\n')
              cat('Generator:\n')
              cat('    Modules in series: ', object@generator$Nms, '\n')
              cat('    Modules in parallel: ', object@generator$Nmp, '\n')
              cat('    Nominal power (kWp): ',
                  round(object@generator$Pg/1000, 1), '\n\n')
          })

#### Methods for prodPVPS ####

### show ###
setMethod('show',
          signature = (object = 'ProdPVPS'),
          definition = function(object){
              callNextMethod()
              cat('-----------------\n')
              cat('Pump:\n')
              cat('    Qn: ', object@pump$Qn, '\n')
              cat('    Stages: ', object@pump$stages, '\n')
              cat('Height (m): ', object@H, '\n')
              cat('Generator (Wp): ', object@Pg, '\n')
          })

#### Methods for Shade ####

### show ###

setMethod('show', 'Shade',
          function(object){
              cat('Object of class ', class(object),'\n\n')
              cat('Source of meteorological information: ')
              cat(paste(object@type, object@source, sep = '-'),'\n\n')
              cat('Latitude of source: ',
                  paste(round(getLat(as(object, 'Meteo'),'deg'), 1),
                        'degrees\n'))
              cat('Latitude for calculations: ',
                  paste(round(getLat(object, 'deg'),1), 'degrees\n\n'))
              cat('Monthly avarages:\n')
              cat('Dimensions of structure:\n')
              print(object@struct)
              cat('Shade calculation mode:\n')
              print(object@modeShd)
              cat('Productivity without shadows:\n')
              print(as(object, 'ProdGCPV'))##Referencia, sin sombras
              cat('Summary of results:\n')
              print(summary(as.data.table(object)))
          }
          )

### as.data.table ###
setMethod('as.data.table', 'Shade',
          function(x, ...){
              res <- cbind(x@distances,
                           data.table(FS = x@FS, GRR = x@GRR, Yf = x@Yf)
                           )
            return(res)
          }
          )
