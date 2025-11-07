utils::globalVariables(c('.', 'Dates', 'Bo0d', 'Bo0m', 'G0d',
                         'Bo0', 'Ta', '..cols', 'est_SIAR',
                         'Fecha_Instalacion', 'Fecha_Baja',
                         'Latitud', 'Longitud', 'peso', 'Estacion',
                         'Codigo', 'req_url_query', 'req_url_path',
                         'request', 'req_perform', 'resp_body_json',
                         'HoraMin', 'Fecha', 'Radiacion', 'TempMin',
                         'TempMax', 'TempMedia', 'Year', 'Mes', 'index'))

#### monthly means of irradiation ####
readG0dm <- function(G0dm, Ta = 25, lat = 0,
                     year = as.POSIXlt(Sys.Date())$year + 1900,
                     promDays = c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13),
                     source = '')
{
    if(missing(lat)){lat <- 0}
    Dates <- as.IDate(paste(year[1], 1:12, promDays, sep = '-'), tz = 'UTC')
    if (length(year)>1){
        for (i in year[-1]){
            x <- as.IDate(paste(i, 1:12, promDays, sep = '-'), tz = 'UTC')
            Dates <- c(Dates, x)
        }
    }
    G0dm.dt <- data.table(Dates = Dates,
                          G0d = G0dm,
                          Ta = Ta)
    setkey(G0dm.dt, 'Dates')
    results <- new(Class = 'Meteo',
                   latm = lat,
                   data = G0dm.dt,
                   type = 'prom',
                   source = source)
}

#### file to Meteo (daily) ####
readBDd <- function(file, lat,
                    format = "%d/%m/%Y",header = TRUE,
                    fill = TRUE, dec = '.', sep = ';',
                    dates.col = 'Dates', ta.col = 'Ta',
                    g0.col = 'G0', keep.cols = FALSE, ...)
{
    #stops if the arguments are not characters or numerics
    stopifnot(is.character(dates.col) || is.numeric(dates.col))
    stopifnot(is.character(ta.col) || is.numeric(ta.col))
    stopifnot(is.character(g0.col) || is.numeric(g0.col))

    #read from file and set it in a data.table
    bd <- fread(file, header = header, fill = fill, dec = dec, sep = sep, ...)

    if(dates.col == ''){
        names(bd)[1] <- 'Dates'
        dates.col <- 'Dates'
    }
    
    #check the columns
    if(!(dates.col %in% names(bd))) stop(paste('The column', dates.col, 'is not in the file'))
    if(!(g0.col %in% names(bd))) stop(paste('The column', g0.col, 'is not in the file'))
    if(!(ta.col %in% names(bd))) stop(paste('The column', ta.col, 'is not in the file'))
    
    #name the dates column by Dates
    Dates <- bd[[dates.col]]
    bd[,(dates.col) := NULL]
    bd[, Dates := as.IDate(Dates, format = format)]

    #name the g0 column by G0
    G0 <- bd[[g0.col]]
    bd[, (g0.col) := NULL]
    bd[, G0 := as.numeric(G0)]
    
    #name the ta column by Ta
    Ta <- bd[[ta.col]]
    bd[, (ta.col) := NULL]
    bd[, Ta := as.numeric(Ta)]

    names0 <- NULL
    if(all(c('D0', 'B0') %in% names(bd))){
        names0 <- c(names0, 'D0', 'B0')
    }

    names0 <- c(names0, 'Ta')

    if(all(c('TempMin', 'TempMax') %in% names(bd))){
        names0 <- c(names0, 'TempMin', 'TempMax')
    }
    if(keep.cols)
    {
        #keep the rest of the columns but reorder the columns
        setcolorder(bd, c('Dates', 'G0', names0))
    }
    else
    {
        #erase the rest of the columns
        cols <- c('Dates', 'G0', names0)
        bd <- bd[, ..cols]
    }

    setkey(bd, 'Dates')
    result <- new(Class = 'Meteo',
                  latm = lat,
                  data = bd,
                  type = 'bd',
                  source = file)
}

#### file to Meteo (intradaily) ####
readBDi <- function(file, lat,
                    format = "%d/%m/%Y %H:%M:%S",
                    header = TRUE, fill = TRUE, dec = '.',
                    sep = ';', dates.col = 'Dates', times.col,
                    ta.col = 'Ta', g0.col = 'G0', keep.cols = FALSE, ...)
{
    #stops if the arguments are not characters or numerics
    stopifnot(is.character(dates.col) || is.numeric(dates.col))
    stopifnot(is.character(ta.col) || is.numeric(ta.col))
    stopifnot(is.character(g0.col) || is.numeric(g0.col))

    #read from file and set it in a data.table
    bd <- fread(file, header = header, fill = fill, dec = dec, sep = sep, ...)

    if(dates.col == ''){
        names(bd)[1] <- 'Dates'
        dates.col <- 'Dates'
    }
    
    #check the columns
    if(!(dates.col %in% names(bd))) stop(paste('The column', dates.col, 'is not in the file'))
    if(!(g0.col %in% names(bd))) stop(paste('The column', g0.col, 'is not in the file'))
    if(!(ta.col %in% names(bd))) stop(paste('The column', ta.col, 'is not in the file'))
    
    if(!missing(times.col)){
        stopifnot(is.character(times.col) || is.numeric(times.col))
        if(!(times.col %in% names(bd))) stop(paste('The column', times.col, 'is not in the file'))
 
        #name the dates column by Dates
        format <- strsplit(format, ' ')
        dd <- as.IDate(bd[[dates.col]], format = format[[1]][1])
        tt <- as.ITime(bd[[times.col]], format = format[[1]][2])
        bd[,(dates.col) := NULL]
        bd[,(times.col) := NULL]
        bd[, Dates := as.POSIXct(dd, tt, tz = 'UTC')]
    }

    else
    {
        dd <- as.POSIXct(bd[[dates.col]], format = format, tz = 'UTC')
        bd[, (dates.col) := NULL]
        bd[, Dates := dd]
    }

    #name the g0 column by G0
    G0 <- bd[[g0.col]]
    bd[, (g0.col) := NULL]
    bd[, G0 := as.numeric(G0)]
    
    #name the ta column by Ta
    Ta <- bd[[ta.col]]
    bd[, (ta.col) := NULL]
    bd[, Ta := as.numeric(Ta)]

    names0 <- NULL
    if(all(c('D0', 'B0') %in% names(bd))){
        names0 <- c(names0, 'D0', 'B0')
    }

    names0 <- c(names0, 'Ta')
    
    if(keep.cols)
    {
        #keep the rest of the columns but reorder the columns
        setcolorder(bd, c('Dates', 'G0', names0))
    }
    else
    {
        #erase the rest of the columns
        cols <- c('Dates', 'G0', names0)
        bd <- bd[, ..cols]
    }
    
    setkey(bd, 'Dates')
    result <- new(Class = 'Meteo',
                  latm = lat,
                  data = bd,
                  type = 'bdI',
                  source = file)
}


dt2Meteo <- function(file, lat, source = '', type){
    if(missing(lat)) stop('lat is missing')

    if(source == '') source <- class(file)[1]
    
    ## Make sure its a data.table
    bd <- data.table(file)

    ## Dates is an as.POSIX element
    bd[, Dates := as.POSIXct(Dates, tz = 'UTC')]

    ## type
    if(missing(type)){
        sample <- median(diff(bd$Dates))
        IsDaily <- as.numeric(sample, units = 'days')
        if(is.na(IsDaily)) IsDaily <- ifelse('G0d' %in% names(bd),
                                             1, 0)
        if(IsDaily >= 30) type <- 'prom'
        else{
            type <- ifelse(IsDaily >= 1, 'bd', 'bdI') 
        }
        
    }
    ## Columns of the data.table
    nms0 <- switch(type,
                   bd = ,
                   prom = {
                       nms0 <- 'G0d'
                       if(all(c('D0d', 'B0d') %in% names(bd))){
                           nms0 <- c(nms0, 'D0d', 'B0d')
                       }
                       if('Ta' %in% names(bd)) nms0 <- c(nms0, 'Ta')
                       if(all(c('TempMin', 'TempMax') %in% names(bd))){
                           nms0 <- c(nms0, 'TempMin', 'TempMax')
                       }
                       nms0
                   },
                   bdI = {
                       nms0 <- 'G0'
                       if(all(c('D0', 'B0') %in% names(bd))){
                           nms0 <- c(nms0, 'D0', 'B0')
                       }
                       if('Ta' %in% names(bd)) nms0 <- c(nms0, 'Ta')
                       nms0
                   })
    ## Columns order and set key
    setcolorder(bd, c('Dates', nms0))
    setkey(bd, 'Dates')
    ## Result
    result <- new(Class = 'Meteo',
                  latm = lat,
                  data = bd,
                  type = type,
                  source = source)
    
    if(!('Ta' %in% names(bd))){
        if(all(c('TempMin', 'TempMax') %in% names(bd))){
            sol <- calcSol(lat = lat, BTi = indexD(result))
            bd[, Ta := fTemp(sol, result)$Ta]
        }
        else bd[, Ta := 25]
        result@data <- bd
    }
    return(result)
}

#### Liu and Jordan, Collares-Pereira and Rabl proposals ####
collper <- function(sol, compD)
{
    Dates <- indexI(sol)
    x <- as.Date(Dates)
    ind.rep <- cumsum(c(1, diff(x) != 0))
    solI <- as.data.tableI(sol, complete = T)
    ws <- solI$ws
    w <- solI$w

    a <- 0.409-0.5016*sin(ws+pi/3)
    b <- 0.6609+0.4767*sin(ws+pi/3)

    rd <- solI[, Bo0/Bo0d]
    rg <- rd * (a + b * cos(w))

    # Daily irradiation components
    G0d <- compD$G0d[ind.rep]
    B0d <- compD$B0d[ind.rep]
    D0d <- compD$D0d[ind.rep]

    # Daily profile
    G0 <- G0d * rg
    D0 <- D0d * rd
    
    # This method may produce diffuse irradiance higher than
    # global irradiance
    G0 <- pmax(G0, D0, na.rm = TRUE)
    B0 <- G0 - D0

    # Negative values are set to NA
    neg <- (B0 < 0) | (D0 < 0) | (G0 < 0)
    is.na(G0) <- neg
    is.na(B0) <- neg
    is.na(D0) <- neg
    
    # Daily profiles are scaled to keep daily irradiation values
    day <- truncDay(indexI(sol))
    sample <- sol@sample

    G0dCP <- ave(G0, day, FUN=function(x) P2E(x, sample))
    B0dCP <- ave(B0, day, FUN=function(x) P2E(x, sample))
    D0dCP <- ave(D0, day, FUN=function(x) P2E(x, sample))
    
    G0 <- G0 * G0d/G0dCP
    B0 <- B0 * B0d/B0dCP
    D0 <- D0 * D0d/D0dCP
    
    res <- data.table(G0, B0, D0)
    return(res)
}


#### intradaily Meteo to daily Meteo ####
Meteoi2Meteod <- function(G0i)
{
    lat <- G0i@latm
    source <- G0i@source

    dt0 <- getData(G0i)
    dt <- dt0[, lapply(.SD, sum, na.rm = TRUE),
              .SDcols = 'G0',
              by = .(Dates = as.IDate(Dates))]
    if('Ta' %in% names(dt0)){
        Ta <- dt0[, .(Ta = mean(Ta),
                      TempMin = min(Ta),
                      TempMax = max(Ta)),
                  by = .(Dates = as.IDate(Dates))]
        if(all(Ta$Ta == c(Ta$TempMin, Ta$TempMax))) Ta[, c('TempMin', 'TempMax') := NULL]
        dt <- merge(dt, Ta)
    }
    if('G0' %in% names(dt)){
        names(dt)[names(dt) == 'G0'] <- 'G0d'
    }
    if('D0' %in% names(dt)){
        names(dt)[names(dt) == 'D0'] <- 'D0d'
    }
    if('B0' %in% names(dt)){
        names(dt)[names(dt) == 'B0'] <- 'B0d'
    }
    G0d <- dt2Meteo(dt, lat, source, type = 'bd')
    return(G0d)
}

#### daily Meteo to monthly Meteo ####
Meteod2Meteom <- function(G0d)
{
    lat <- G0d@latm
    source <- G0d@source

    dt <- getData(G0d)
    nms <- names(dt)[-1]
    dt <- dt[, lapply(.SD, mean),
             .SDcols = nms,
             by = .(month(Dates), year(Dates))]
    dt[, Dates := fBTd()]
    dt <- dt[, c('month', 'year') := NULL]
    
    setcolorder(dt, 'Dates')

    G0m <- dt2Meteo(dt, lat, source, type = 'prom')
    return(G0m)
}

zoo2Meteo <- function(file, lat, source = '')
{
    if(source == ''){
        name <- deparse(substitute(file))
        cl <- class(file)
        source <- paste(cl, name, sep = '-')
    }
    bd <- data.table(file)
    sample <- median(diff(index(file)))
    IsDaily <- as.numeric(sample, units = 'days')>=1
    type <- ifelse(IsDaily, 'bd', 'bdI')
    result <- new(Class = 'Meteo',
                  latm = lat,
                  data = bd,
                  type = type,
                  source = source)
}

siarGET <- function(id, inicio, final, tipo = 'Mensuales', ambito = 'Estacion'){
    if(!(tipo %in% c('Horarios', 'Diarios', 'Semanales', 'Mensuales'))){
        stop('argument \'tipo\' must be: Horarios, Diarios, Semanales or Mensuales')
    }
    if(!(ambito %in% c('CCAA', 'Provincia', 'Estacion'))){
        stop('argument \'ambito\' must be: CCAA, Provincia or Estacion')
    }
    
    mainURL <- "https://servicio.mapama.gob.es"

    path <- paste('/apisiar/API/v1/Datos', tipo, ambito, sep = '/')

    ## prepare the APIsiar
    req <- request(mainURL) |>
        req_url_path(path) |>
        req_url_query(Id = id,
                      FechaInicial = inicio,
                      FechaFinal = final,
                      ClaveAPI = '_Q8L_niYFBBmBs-vB3UomUqdUYy98FTRX1aYbrZ8n2FXuHYGTV')
    ## execute it
    resp <- req_perform(req)

    ##JSON to R
    respJSON <- resp_body_json(resp, simplifyVector = TRUE)

    if(!is.null(respJSON$MensajeRespuesta)){
        stop(respJSON$MensajeRespuesta)
    }

    res0 <- data.table(respJSON$Datos)

    res <- switch(tipo,
                  Horarios = {
                      res0[, HoraMin := as.ITime(sprintf('%04d', HoraMin),
                                                 format = '%H%M')]
                      res0[, Fecha := as.IDate(Fecha, format = '%Y-%m-%d')]
                      res0[, Fecha := as.IDate(ifelse(HoraMin == as.ITime(0),
                                                      Fecha+1, Fecha))]
                      res0[, Dates := as.POSIXct(HoraMin, Fecha,
                                                 tz = 'Europe/Madrid')]
                      res0 <- res0[, .(Dates,
                                       G0 = Radiacion,
                                       Ta = TempMedia)]
                      return(res0)
                  },
                  Diarios = {
                      res0[, Dates := as.IDate(Fecha)]
                      res0 <- res0[, .(Dates,
                                       G0d = Radiacion * 277.78,
                                       Ta = TempMedia,
                                       TempMin,
                                       TempMax)]
                      return(res0)
                  },
                  Semanales = res0,
                  Mensuales = {
                      promDays<-c(17,14,15,15,15,10,18,18,18,19,18,13)
                      names(res0)[1] <- 'Year'
                      res0[, Dates := as.IDate(paste(Year, Mes,
                                                     promDays[Mes],
                                                     sep = '-'))]
                      res0 <- res0[, .(Dates,
                                       G0d = Radiacion * 277.78,
                                       Ta = TempMedia,
                                       TempMin,
                                       TempMax)]
                  })
    
    return(res)
}

haversine <- function(lat1, lon1, lat2, lon2) {
    R <- 6371  # Radius of the Earth in kilometers
    dLat <- (lat2 - lat1) * pi / 180
    dLon <- (lon2 - lon1) * pi / 180
    a <- sin(dLat / 2) * sin(dLat / 2) + cos(lat1 * pi / 180) *
        cos(lat2 * pi / 180) * sin(dLon / 2) * sin(dLon / 2)
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    d <- R * c
    return(d)
}

readSIAR <- function(Lon = 0, Lat = 0,
                     inicio = paste(year(Sys.Date())-1, '01-01', sep = '-'),
                     final = paste(year(Sys.Date())-1, '12-31', sep = '-'),
                     tipo = 'Mensuales', n_est = 3){
    inicio <- as.Date(inicio)
    final <- as.Date(final)
    
    n_reg <- switch(tipo,
                    Horarios = {
                        tt <- difftime(final, inicio, units = 'days')
                        tt <- (as.numeric(tt)+1)*48
                        tt <- tt*n_est
                        tt
                    },
                    Diarios = {
                        tt <- difftime(final, inicio, units = 'days')
                        tt <- as.numeric(tt)+1
                        tt <- tt*n_est
                        tt
                    },
                    Semanales = {
                        tt <- difftime(final, inicio, units = 'weeks')
                        tt <- as.numeric(tt)
                        tt <- tt*n_est
                        tt
                    },
                    Mensuales = {
                        tt <- difftime(final, inicio, units = 'weeks')
                        tt <- as.numeric(tt)/4.34524
                        tt <- ceiling(tt)
                        tt <- tt*n_est
                        tt
                    })
    if(n_reg > 100) stop(paste('Number of requested records (', n_reg,
                                ') exceeds the maximum allowed (100)', sep = ''))
    ## Obtain the nearest stations
    siar <- est_SIAR[
        Fecha_Instalacion <= final & (is.na(Fecha_Baja) | Fecha_Baja >= inicio)
    ]

    ## Weigths for the interpolation
    siar[, dist := haversine(Latitud, Longitud, Lat, Lon)]
    siar <- siar[order(dist)][1:n_est]
    siar[, peso := 1/dist]
    siar[, peso := peso/sum(peso)]
    ## Is the given location within the polygon formed by the stations?
    siar <- siar[, .(Estacion, Codigo, dist, peso)]

    ## List for the data.tables of siarGET
    siar_list <- list()
    for(codigo in siar$Codigo){
        siar_list[[codigo]] <- siarGET(id = codigo,
                                       inicio = as.character(inicio),
                                       final = as.character(final),
                                       tipo = tipo)
        siar_list[[codigo]]$peso <- siar[Codigo == codigo, peso]
    }

    ## Bind the data.tables
    s_comb <- rbindlist(siar_list, use.names = TRUE, fill = TRUE)

    nms <- names(s_comb)
    nms <- nms[-c(1, length(nms))]

    ## Interpole
    res <- s_comb[, lapply(.SD * peso, sum, na.rm = TRUE),
                  .SDcols = nms,
                  by = Dates]

    ## Source
    mainURL <- "https://servicio.mapama.gob.es"
    Estaciones <- siar[, paste(Estacion, '(', Codigo, ')', sep = '')]
    Estaciones <- paste(Estaciones, collapse = ', ')
    source <- paste(mainURL, '\n  -Estaciones:', Estaciones, sep = ' ')
    
    res <- switch(tipo,
                  Horarios = {dt2Meteo(res, lat = Lat, source = mainURL, type = 'bdI')},
                  Diarios = {dt2Meteo(res, lat = Lat, source = mainURL, type = 'bd')},
                  Semanales = {res},
                  Mensuales = {dt2Meteo(res, lat = Lat, source = source, type = 'prom')})
    return(res)
}
