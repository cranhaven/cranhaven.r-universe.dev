utils::globalVariables('Qd')

prodPVPS<-function(lat, 
                   modeTrk = 'fixed', 
                   modeRad = 'prom', 
                   dataRad,
                   sample = 'hour',
                   keep.night = TRUE,
                   sunGeometry = 'michalsky',
                   corr, f,
                   betaLim = 90, beta = abs(lat)-10, alpha  =  0,
                   iS = 2, alb = 0.2, horizBright = TRUE, HCPV = FALSE,
                   pump , H, 
                   Pg, converter =  list(), 
                   effSys = list(),
                   ...){
    
    stopifnot(is.list(converter),
              is.list(effSys))
    
    if (modeRad!='prev'){ #We do not use a previous calculation
        
        radEf <- calcGef(lat = lat, modeTrk = modeTrk, modeRad = modeRad,
                         dataRad = dataRad,
                         sample = sample, keep.night = keep.night,
                         sunGeometry = sunGeometry,
                         corr = corr, f = f,
                         betaLim = betaLim, beta = beta, alpha = alpha,
                         iS = iS, alb = alb, horizBright = horizBright, HCPV = HCPV,
                         modeShd = '', ...)
        
    } else { #We use a previous calculation of calcG0, calcGef or prodPVPS
        stopifnot(class(dataRad) %in% c('G0', 'Gef', 'ProdPVPS'))
        radEf <- switch(class(dataRad),
                        G0 = calcGef(lat = lat, 
                                     modeTrk = modeTrk, modeRad = 'prev',
                                     dataRad = dataRad,
                                     betaLim = betaLim, beta = beta, alpha = alpha,
                                     iS = iS, alb = alb, horizBright = horizBright,
                                     HCPV = HCPV, modeShd = '', ...),
                        Gef = dataRad,
                        ProdPVPS = as(dataRad, 'Gef')
                        )
    }
    
###Electric production
    converter.default <- list(Ki = c(0.01,0.025,0.05), Pnom = Pg)
    converter <- modifyList(converter.default, converter)
    
    effSys.default <- list(ModQual = 3,ModDisp = 2,OhmDC = 1.5,OhmAC = 1.5,MPP = 1,TrafoMT = 1,Disp = 0.5)
    effSys <- modifyList(effSys.default, effSys)
    
    TONC <- 47
    Ct <- (TONC-20)/800
    lambda <- 0.0045
    Gef <- radEf@GefI$Gef
    night <- radEf@solI$night
    Ta <- radEf@Ta$Ta
    
    Tc <- Ta+Ct*Gef
    Pdc <- Pg*Gef/1000*(1-lambda*(Tc-25))
    Pdc[is.na(Pdc)] <- 0 #Necessary for the functions provided by fPump
    PdcN <- with(effSys,
                 Pdc/converter$Pnom*(1-ModQual/100)*(1-ModDisp/100)*(1-OhmDC/100)
                 )
    PacN <- with(converter,{
        A <- Ki[3]
        B <- Ki[2]+1
        C <- Ki[1]-(PdcN)
        ##AC power normalized to the inverter
        result <- (-B+sqrt(B^2-4*A*C))/(2*A)
    })
    PacN[PacN<0]<-0
    
    Pac <- with(converter,
                PacN*Pnom*(1-effSys$OhmAC/100))
    Pdc <- PdcN*converter$Pnom*(Pac>0)
    
    
###Pump
    fun<-fPump(pump = pump, H = H)
    ##I limit power to the pump operating range.
    rango <- with(fun,Pac>=lim[1] & Pac<=lim[2]) 
    Pac[!rango]<-0
    Pdc[!rango]<-0
    prodI <- data.table(Pac = Pac,Pdc = Pdc,Q = 0,Pb = 0,Ph = 0,f = 0)	
    prodI <- within(prodI,{
        Q[rango]<-fun$fQ(Pac[rango])
        Pb[rango]<-fun$fPb(Pac[rango])
        Ph[rango]<-fun$fPh(Pac[rango])
        f[rango]<-fun$fFreq(Pac[rango])
        etam <- Pb/Pac
        etab <- Ph/Pb
    })
    
    prodI[night,]<-NA
    prodI[, Dates := indexI(radEf)]
    setcolorder(prodI, c('Dates', names(prodI)[-length(prodI)]))
    
###daily, monthly and yearly values
    
    by <- radEf@sample
    
    if(radEf@type == 'prom'){
        prodDm <- prodI[, .(Eac = P2E(Pac, by)/1000,
                            Qd = P2E(Q, by)),
                        by = .(month(Dates), year(Dates))]
        prodDm[, Yf := Eac/(Pg/1000)]

        prodD <- prodDm[, .(Eac = Eac*1000,
                            Qd,
                            Yf),
                        by = .(Dates = indexD(radEf))]

        prodDm[, DayOfMonth := DOM(prodDm)]
        
        prody <- prodDm[, lapply(.SD*DayOfMonth, sum, na.rm = TRUE),
                        .SDcols = c('Eac', 'Qd', 'Yf'),
                        by = .(Dates = year)]
        prodDm[, DayOfMonth := NULL]
    } else {
        prodD <- prodI[, .(Eac = P2E(Pac, by)/1000,
                           Qd = P2E(Q, by)),
                       by = .(Dates = truncDay(Dates))]
        prodD[, Yf := Eac/Pg*1000]

        prodDm <- prodD[, lapply(.SD, mean, na.rm = TRUE),
                        .SDcols = c('Eac','Qd', 'Yf'),
                        by = .(month(Dates), year(Dates))]
        prody <- prodD[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = c('Eac', 'Qd', 'Yf'),
                       by = .(Dates = year(Dates))]
        
    }

    promDays = c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    prodDm[, Dates := as.Date(paste(year, month,
                                    promDays[month], sep = '-'))]
    prodDm[, c('month', 'year') := NULL]
    setcolorder(prodDm, 'Dates')
    
    result <- new('ProdPVPS',
                  radEf,                  #contains 'Gef'
                  prodD = prodD,
                  prodDm = prodDm,
                  prody = prody,
                  prodI = prodI,
                  pump = pump,
                  H = H,
                  Pg = Pg,
                  converter = converter,
                  effSys = effSys
                  )
}

