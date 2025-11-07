utils::globalVariables(c('Yf', 'Eac'))

prodGCPV<-function(lat,
                   modeTrk = 'fixed', 
                   modeRad = 'prom',
                   dataRad,
                   sample = 'hour',
                   keep.night = TRUE,
                   sunGeometry = 'michalsky',
                   corr, f,
                   betaLim = 90, beta = abs(lat)-10, alpha = 0,
                   iS = 2, alb = 0.2, horizBright = TRUE, HCPV = FALSE,
                   module = list(), 
                   generator = list(),
                   inverter = list(), 
                   effSys = list(), 
                   modeShd = '',    
                   struct = list(), 
                   distances = data.table(),
                   ...){
    
    stopifnot(is.list(module),
              is.list(generator),
              is.list(inverter),
              is.list(effSys),
              is.list(struct),
              is.data.table(distances))
    
  if (('bt' %in% modeShd) & (modeTrk!='horiz')) {
      modeShd[which(modeShd=='bt')] = 'area'
      warning('backtracking is only implemented for modeTrk = horiz')}
    
    if (modeRad!='prev'){ #We do not use a previous calculation
        
    radEf <- calcGef(lat = lat, modeTrk = modeTrk, modeRad = modeRad,
                     dataRad = dataRad,
                     sample = sample, keep.night = keep.night,
                     sunGeometry = sunGeometry,
                     corr = corr, f = f,
                     betaLim = betaLim, beta = beta, alpha = alpha,
                     iS = iS, alb = alb, horizBright = horizBright, HCPV = HCPV,
                     modeShd = modeShd, struct = struct, distances = distances, ...)
        
    } else { #We use a previous calcG0, calcGef or prodGCPV calculation.
      
        stopifnot(class(dataRad) %in% c('G0', 'Gef', 'ProdGCPV'))
        radEf <- switch(class(dataRad),
                        G0 = calcGef(lat = lat,
                                     modeTrk = modeTrk, modeRad = 'prev',
                                     dataRad = dataRad,
                                     betaLim = betaLim, beta = beta, alpha = alpha,
                                     iS = iS, alb = alb, horizBright = horizBright, HCPV = HCPV,
                                     modeShd = modeShd, struct = struct, distances = distances, ...),
                        Gef = dataRad,
                        ProdGCPV = as(dataRad, 'Gef')
                        )
    }
    
    
    ##Production
    prodI <- fProd(radEf,module,generator,inverter,effSys)
    module <- attr(prodI, 'module')
    generator <- attr(prodI, 'generator')
    inverter <- attr(prodI, 'inverter')
    effSys <- attr(prodI, 'effSys')
    
    ##Calculation of daily, monthly and annual values
    Pg <- generator$Pg #Wp
    
    by <- radEf@sample
    nms1 <- c('Pac', 'Pdc')
    nms2 <- c('Eac', 'Edc', 'Yf')
    
    
    if(radEf@type == 'prom'){
        prodDm <- prodI[, lapply(.SD/1000, P2E, by),
                        .SDcols = nms1,
                        by = .(month(Dates), year(Dates))]
        names(prodDm)[-c(1,2)] <- nms2[-3]
        prodDm[, Yf := Eac/(Pg/1000)]
        prodD <- prodDm[, .SD*1000,
                        .SDcols = nms2,
                        by = .(Dates = indexD(radEf))]
        prodD[, Yf := Yf/1000]
        
        prodDm[, DayOfMonth := DOM(prodDm)]
        prody <- prodDm[, lapply(.SD*DayOfMonth, sum, na.rm = TRUE),
                        .SDcols = nms2,
                        by = .(Dates = year)]
        prodDm[, DayOfMonth := NULL]
    } else {
        prodD <- prodI[, lapply(.SD, P2E, by),
                       .SDcols = nms1,
                       by = .(Dates = truncDay(Dates))]
        names(prodD)[-1] <- nms2[-3]
        prodD[, Yf := Eac/Pg]

        prodDm <- prodD[, lapply(.SD/1000, mean, na.rm = TRUE),
                        .SDcols = nms2,
                        by = .(month(Dates), year(Dates))]
        prodDm[, Yf := Yf * 1000]
        prody <- prodD[, lapply(.SD/1000, sum, na.rm = TRUE),
                       .SDcols = nms2,
                       by = .(Dates = year(Dates))]
        prody[, Yf := Yf * 1000]
    }
    
    promDays = c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    prodDm[, Dates := as.Date(paste(year, month,
                                    promDays[month], sep = '-'))]
    prodDm[, c('month', 'year') := NULL]
    setcolorder(prodDm, 'Dates')
    
    result <- new('ProdGCPV',
                  radEf,                  #contains 'Gef'
                  prodD = prodD,
                  prodDm = prodDm,
                  prody = prody,
                  prodI = prodI,
                  module = module,
                  generator = generator,
                  inverter = inverter,
                  effSys = effSys
                  )
}
