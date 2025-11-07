calcGef<-function(lat,
                  modeTrk = 'fixed',      #c('two','horiz','fixed')
                  modeRad = 'prom', 
                  dataRad,
                  sample = 'hour',
                  keep.night = TRUE,
                  sunGeometry = 'michalsky',
                  corr, f,
                  betaLim = 90, beta = abs(lat)-10, alpha = 0,
                  iS = 2, alb = 0.2, horizBright = TRUE, HCPV = FALSE,
                  modeShd = '',    #modeShd = c('area','bt','prom')
                  struct = list(), #list(W = 23.11, L = 9.8, Nrow = 2, Ncol = 8), 
                  distances = data.table(),#data.table(Lew = 40, Lns = 30, H = 0)){
                  ...){
    
    stopifnot(is.list(struct), is.data.frame(distances))
    
    if (('bt' %in% modeShd) & (modeTrk!='horiz')) {
        modeShd[which(modeShd=='bt')] = 'area'
        warning('backtracking is only implemented for modeTrk = horiz')}
    
    if (modeRad!='prev'){ #not use a prev calculation
        radHoriz <- calcG0(lat = lat, modeRad = modeRad,
                           dataRad = dataRad,
                           sample = sample, keep.night = keep.night,
                           sunGeometry = sunGeometry,
                           corr = corr, f = f, ...)
    } else {                          #use a prev calculation
        radHoriz <- as(dataRad, 'G0') 
    } 
    
### Inclined and effective radiation
    BT = ("bt" %in% modeShd) 
    angGen <- fTheta(radHoriz, beta, alpha, modeTrk, betaLim, BT, struct, distances)
    inclin <- fInclin(radHoriz, angGen, iS, alb, horizBright, HCPV)
    
### Daily, monthly and yearly values
    by <- radHoriz@sample
    nms <- c('Bo', 'Bn', 'G', 'D', 'B', 'Gef', 'Def', 'Bef')
    nmsd <- paste(nms, 'd', sep = '')

    
    if(radHoriz@type == 'prom'){
        Gefdm <- inclin[, lapply(.SD/1000, P2E, by),
                        .SDcols = nms,
                        by = .(month(Dates), year(Dates))]
        names(Gefdm)[-c(1,2)] <- nmsd
        GefD <- Gefdm[, .SD*1000,
                      .SDcols = nmsd,
                      by = .(Dates = indexD(radHoriz))]
       
        Gefdm[, DayOfMonth := DOM(Gefdm)]
        Gefy <- Gefdm[, lapply(.SD*DayOfMonth, sum, na.rm = TRUE),
                      .SDcols = nmsd,
                      by = .(Dates = year)]
        Gefdm[, DayOfMonth := NULL]
    } else{
        GefD <- inclin[, lapply(.SD, P2E, by),
                       .SDcols = nms,
                       by = .(Dates = truncDay(Dates))]
        names(GefD)[-1] <- nmsd

        Gefdm <- GefD[, lapply(.SD/1000, mean, na.rm = TRUE),
                      .SDcols = nmsd,
                      by = .(month(indexD(radHoriz)), year(indexD(radHoriz)))]
        Gefy <- GefD[, lapply(.SD/1000, sum, na.rm = TRUE),
                     .SDcols = nmsd,
                     by = .(Dates = year(indexD(radHoriz)))]
    }

    promDays = c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    Gefdm[, Dates := as.Date(paste(year, month,
                                   promDays[month], sep = '-'))]
    Gefdm[, c('month', 'year') := NULL]
    setcolorder(Gefdm, 'Dates')
    
###Resultado antes de sombras
    result0 = new('Gef',
                radHoriz,                           #Gef contains 'G0'
                Theta = angGen,
                GefD = GefD,
                Gefdm = Gefdm,
                Gefy = Gefy,
                GefI = inclin,
                iS = iS,
                alb = alb,
                modeTrk = modeTrk,
                modeShd = modeShd,
                angGen = list(alpha = alpha, beta = beta, betaLim = betaLim),
                struct = struct,
                distances = distances
                )
###Shadows
    if (isTRUE(modeShd == "") ||        #If modeShd=='' there is no shadow calculation
        ('bt' %in% modeShd)) {            #nor if there is backtracking
        return(result0)
    } else {
        result <- calcShd(result0, modeShd, struct, distances)
        return(result)
    }
}
