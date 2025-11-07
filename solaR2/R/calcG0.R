utils::globalVariables('DayOfMonth')

calcG0 <- function(lat,
                   modeRad = 'prom',
                   dataRad,
                   sample = 'hour',
                   keep.night = TRUE,
                   sunGeometry = 'michalsky',
                   corr, f, ...)
{
    
    if (missing(lat)) stop('lat missing. You must provide a latitude value.')
    
    stopifnot(modeRad %in% c('prom', 'aguiar','bd', 'bdI'))
    

###Radiation data
    if (missing(corr)){
        corr = switch(modeRad,
                      bd = 'CPR', #Correlation between Fd and Kt for daily values
                      aguiar = 'CPR', #Correlation between Fd and Kt for daily values
                      prom = 'Page',  #Correlation between Fd and Kt for monthly averages
                      bdI = 'BRL'     #Correlation between fd and kt for intraday values
                      )
    }
    
    if(is(dataRad, 'Meteo')){BD <- dataRad}
    else{
    BD <- switch(modeRad,
                 bd = {
                     if (!is.list(dataRad)||is.data.frame(dataRad)){
                         dataRad <- list(file = dataRad)
                     }
                         switch(class(dataRad$file)[1],
                                character = {
                                    bd.default = list(file = '', lat = lat)
                                    bd = modifyList(bd.default, dataRad)
                                    res <- do.call('readBDd', bd)
                                    res
                                },
                                data.table = ,
                                data.frame = {
                                    bd.default = list(file = '', lat = lat)
                                    bd = modifyList(bd.default, dataRad)
                                    res <- do.call('dt2Meteo', bd)
                                    res
                                },
                                zoo = {
                                    bd.default = list(file = '', lat = lat, source = '')
                                    bd = modifyList(bd.default, dataRad)
                                    res <- do.call('zoo2Meteo', bd)
                                    res
                                })
                     }, #End of bd
                 prom = {
                     if (!is.list(dataRad)) dataRad <- list(G0dm = dataRad)
                     prom.default <- list(G0dm = numeric(), lat = lat)
                     prom = modifyList(prom.default, dataRad)
                     res <- do.call('readG0dm', prom)
                 }, #End of prom
                 aguiar = {
                     if (is.list(dataRad)){
                         if('year' %in% names(dataRad))
                             BTd <- fBTd(mode = 'serie', year = dataRad$year)
                         else
                             BTd <- fBTd(mode = 'serie')
                         dataRad <- dataRad$G0dm
                     }
                     else
                         BTd <- fBTd(mode = 'serie')
                     solD <- fSolD(lat, BTd)
                     G0d <- markovG0(dataRad, solD)
                     res <- dt2Meteo(G0d, lat = lat, source = 'aguiar')
                 }, #End of aguiar
                 bdI = {
                     if (!is.list(dataRad) || is.data.frame(dataRad)){
                         dataRad <- list(file = dataRad)
                     }
                     switch(class(dataRad$file)[1],
                            character = {
                                bdI.default <- list(file = '', lat = lat)
                                bdI <- modifyList(bdI.default, dataRad)
                                res <- do.call('readBDi', bdI)
                                res
                            },
                            data.table = ,
                            data.frame = {
                                bdI.default <- list(file = '', lat = lat)
                                bdI <- modifyList(bdI.default, dataRad)
                                res <- do.call('dt2Meteo', bdI)
                                res
                            },
                            zoo = {
                                bdI.default <- list(file = '', lat = lat, source = '')
                                bdI <- modifyList(bdI.default, dataRad)
                                res <- do.call('zoo2Meteo', bdI)
                                res
                            },
                            stop('dataRad$file should be a character, a data.table, a data.frame or a zoo.')
                            )} #End of btI
                 )             #End of general switch    
    }
        
    
### Angulos solares y componentes de irradiancia
    if (modeRad == 'bdI') {
        sol <- calcSol(lat, sample = sample,
                       BTi = indexD(BD), keep.night = keep.night, method = sunGeometry)
        compI <- fCompI(sol = sol, G0I = BD, corr = corr, f = f, ...)
        compD <- compI[, lapply(.SD, P2E, sol@sample),
                       .SDcols = c('G0', 'D0', 'B0'),
                       by = truncDay(Dates)]
        names(compD)[1] <- 'Dates'
        names(compD)[-1] <- paste(names(compD)[-1], 'd', sep = '')
        compD$Fd <- compD$D0d/compD$G0d
        compD$Kt <- compD$G0d/as.data.tableD(sol)$Bo0d
    } else { ##modeRad! = 'bdI'
        sol <- calcSol(lat, indexD(BD), sample = sample,
                       keep.night = keep.night, method = sunGeometry)
        compD<-fCompD(sol = sol, G0d = BD, corr = corr, f, ...)
        compI<-fCompI(sol = sol, compD = compD, ...)
    }
    
###Temperature
    
    Ta = switch(modeRad,
                bd = {
                    if (all(c("TempMax","TempMin") %in% names(BD@data))) {
                        fTemp(sol, BD)
                    } else {
                        if ("Ta" %in% names(getData(BD))) {
                            data.table(Dates = indexD(sol),
                                       Ta = getData(BD)$Ta)
                        } else {
                            warning('No temperature information available!')
                        }
                    }
                },
                bdI = {
                    if ("Ta" %in% names(getData(BD))) {
                        data.table(Dates = indexI(sol),
                                   Ta = getData(BD)$Ta)
                    } else {
                        warning('No temperature information available!')
                    }
                },
                prom = {
                    if ("Ta" %in% names(getData(BD))) {
                        data.table(Dates = indexD(sol),
                                   Ta = getData(BD)$Ta)
                    } else {
                        warning('No temperature information available!')
                    }                  
                },
                aguiar = {
                    Dates<-indexI(sol)	
                    x <- as.Date(Dates)
                    ind.rep <- cumsum(c(1, diff(x) != 0))
                    data.table(Dates = Dates,
                               Ta = getData(BD)$Ta[ind.rep])
                }
                )
    
###Medias mensuales y anuales
    nms <- c('G0d', 'D0d', 'B0d')
    G0dm <- compD[, lapply(.SD/1000, mean, na.rm = TRUE),
                  .SDcols = nms,
                  by = .(month(Dates), year(Dates))]
    
    if(modeRad == 'prom'){
        G0dm[, DayOfMonth := DOM(G0dm)]
        G0y <- G0dm[, lapply(.SD*DayOfMonth, sum, na.rm = TRUE),
                    .SDcols = nms,
                    by = .(Dates = year)]
        G0dm[, DayOfMonth := NULL]        
    } else{
        G0y <- compD[, lapply(.SD/1000, sum, na.rm = TRUE),
                     .SDcols = nms,
                     by = .(Dates = year(Dates))]
    }
    promDays = c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    G0dm[, Dates := as.Date(paste(year, month,
                                  promDays[month], sep = '-'))]
    G0dm[, c('month', 'year') := NULL]
    setcolorder(G0dm, 'Dates')
    
###Result
    result <- new(Class = 'G0',
                  BD,        #G0 contains "Meteo"
                  sol,       #G0 contains 'Sol'
                  G0D = compD, #results of fCompD
                  G0dm = G0dm, #monthly means
                  G0y = G0y,   #yearly values
                  G0I = compI, #results of fCompD
                  Ta = Ta      #ambient temperature
                  )
    return(result)
}
