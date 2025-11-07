calcShd<-function(radEf,##class = 'Gef'
                  modeShd = 'prom',      #modeShd = c('area','bt','prom')
                  struct = list(), #list(W = 23.11, L = 9.8, Nrow = 2, Ncol = 8), 
                  distances = data.table() #data.table(Lew = 40, Lns = 30, H = 0)){
                  )
{
    stopifnot(is.list(struct), is.data.frame(distances))

    ##For now I only use modeShd = 'area'
    ##With different modeShd (to be defined) I will be able to calculate Gef in a different way
    ##See macagnan thesis
    prom = ("prom"  %in%  modeShd)
    prev <- as.data.tableI(radEf, complete = TRUE)
    ## shadow calculations
    modeTrk <- radEf@modeTrk
    sol <- data.table(AzS = prev$AzS,
                      AlS = prev$AlS)
    theta <- radEf@Theta
    AngGen <- data.table(theta, sol)
    FS <- fSombra(AngGen, distances, struct, modeTrk, prom)
    ## irradiance calculation
    gef0 <- radEf@GefI
    Bef0 <- gef0$Bef
    Dcef0 <- gef0$Dcef
    Gef0 <- gef0$Gef
    Dief0 <- gef0$Dief
    Ref0 <- gef0$Ref
    ## calculation
    Bef <- Bef0*(1-FS)
    Dcef <- Dcef0*(1-FS)
    Def <- Dief0+Dcef
    Gef <- Dief0+Ref0+Bef+Dcef #Including shadows
    ##Change names
    nms <- c('Gef', 'Def', 'Dcef', 'Bef')
    nmsIndex <- which(names(gef0) %in% nms)
    names(gef0)[nmsIndex]<- paste(names(gef0)[nmsIndex], '0', sep = '')
    GefShd <- gef0
    GefShd[, c(nms, 'FS') := .(Gef, Def, Dcef, Bef, FS)]

    ## daily, monthly and yearly values
    by <- radEf@sample
    nms <- c('Gef0', 'Def0', 'Bef0', 'G', 'D', 'B', 'Gef', 'Def', 'Bef')
    nmsd <- paste(nms, 'd', sep = '')

    Gefdm <- GefShd[, lapply(.SD/1000, P2E, by),
                    by = .(month(truncDay(Dates)), year(truncDay(Dates))),
                    .SDcols = nms]
    names(Gefdm)[-c(1, 2)] <- nmsd

    if(radEf@type == 'prom'){
        GefD <- Gefdm[, .SD[, -c(1, 2)] * 1000,
                      .SDcols = nmsd,
                      by = .(Dates = indexD(radEf))] 
       
        Gefdm[, DayOfMonth := DOM(Gefdm)]
       
        Gefy <- Gefdm[, lapply(.SD*DayOfMonth, sum, na.rm = TRUE),
                      .SDcols = nmsd,
                      by = .(Dates = year)]
        Gefdm[, DayOfMonth := NULL]
    } else{    
        GefD <- GefShd[, lapply(.SD/1000, P2E, by),
                       .SDcols = nms,
                       by = .(Dates = truncDay(Dates))]
        names(GefD)[-1] <- nmsd
            
        Gefy <- GefD[, lapply(.SD[, -1], sum, na.rm = TRUE),
                     .SDcols = nmsd,
                     by = .(Dates = year(Dates))]
    }

    promDays = c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    Gefdm[, Dates := as.Date(paste(year, month,
                                   promDays[month], sep = '-'))]
    Gefdm[, c('month', 'year') := NULL]
    setcolorder(Gefdm, c('Dates', names(Gefdm)[-length(Gefdm)]))

    ## Object of class Gef
    ## modifying the 'modeShd', 'GefI', 'GefD', 'Gefdm', and 'Gefy' slots
    ## from the original radEf object
    radEf@modeShd <- modeShd
    radEf@GefI <- GefShd
    radEf@GefD <- GefD
    radEf@Gefdm <- Gefdm
    radEf@Gefy <- Gefy
    return(radEf)
}
  
