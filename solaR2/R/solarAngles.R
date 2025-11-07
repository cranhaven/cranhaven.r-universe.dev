#### Declination ####
declination <- function(d, method = 'michalsky')
{
    ##Method check
    if(!(method %in% c("michalsky", "cooper", "strous", "spencer"))){
        warning("'method' must be: michalsky, cooper, strous or spencer. Set michalsky")
        method = 'michalsky'
    }

    ## x is an IDate
    d <- as.IDate(d)
    ## Day of year
    dn <- yday(d)
    ## Days from 2000-01-01
    origin <- as.IDate('2000-01-01')
    jd <- as.numeric(d - origin)
    X <- 2 * pi * (dn - 1) / 365

    switch(method,
           michalsky = {
           meanLong <- (280.460 + 0.9856474 * jd)%%360
           meanAnomaly <- (357.528 + 0.9856003 * jd)%%360
           eclipLong <- (meanLong +1.915 * sin(d2r(meanAnomaly)) +
                         0.02 * sin(d2r(2 * meanAnomaly)))%%360
           excen <- 23.439 - 0.0000004 * jd
           sinEclip <- sin(d2r(eclipLong))
           sinExcen <- sin(d2r(excen))
           asin(sinEclip * sinExcen)
           },
           cooper = {
               ##P.I. Cooper. “The Absorption of Solar Radiation in Solar Stills”. Solar Energy 12 (1969).
               d2r(23.45) * sin(2 * pi * (dn +284) / 365)
           },
           strous = {
               meanAnomaly <- (357.5291 + 0.98560028 * jd)%%360
               coefC <- c(1.9148, 0.02, 0.0003)
               sinC <- sin(outer(1:3, d2r(meanAnomaly), '*'))
               C <- colSums(coefC * sinC)
               trueAnomaly <- (meanAnomaly + C)%%360
               eclipLong <- (trueAnomaly + 282.9372)%%360
               excen <- 23.435
               sinEclip <- sin(d2r(eclipLong))
               sinExcen <- sin(d2r(excen))
               asin(sinEclip * sinExcen)
           },
           spencer = {
               ## J.W. Spencer. “Fourier Series Representation of the Position of the Sun”. 2 (1971).
               ##URL: http://www.mail-archive.com/sundial@uni-koeln.de/msg01050.html.
               0.006918 - 0.399912 * cos(X) + 0.070257 * sin(X) -
                   0.006758 * cos(2 * X) + 0.000907 * sin(2 * X) -
                       0.002697 * cos(3 * X) + 0.001480 * sin(3 * X)           
           })
}


#### Eccentricity ####
eccentricity <- function(d, method = 'michalsky')
{
    ##Method check
    if(!(method %in% c("michalsky", "cooper", "strous", "spencer"))){
        warning("'method' must be: michalsky, cooper, strous or spencer. Set michalsky")
        method = 'michalsky'
    }
    
    ##x is an IDate
    d <- as.IDate(d)
    ##Day of year
    dn <- yday(d)
    X <- 2 * pi * (dn-1)/365

    switch(method,
           cooper = 1 + 0.033*cos(2*pi*dn/365),
           spencer = , 
           michalsky = , 
           strous = 1.000110 + 0.034221*cos(X) +
               0.001280*sin(X) + 0.000719*cos(2*X) +
               0.000077*sin(2*X)
           )
}


#### Equation of time

##Alan M.Whitman "A simple expression for the equation of time"
##EoT=ts-t, donde ts es la hora solar real y t es la hora solar
##media. Valores negativos implican que el sol real se retrasa
##respecto al medio
eot <- function(d)
{
    ## d in an IDate
    d <- as.IDate(d)
    ## Day of year
    dn <- yday(d)
    M <- 2 * pi/365.24 * dn
    EoT <- 229.18 * (-0.0334 * sin(M) +
                     0.04184 * sin(2 * M + 3.5884))
    EoT <- h2r(EoT/60)
    return(EoT)
}


#### Solar time ####
sunrise <- function(d, lat, method = 'michalsky',
                    decl = declination(d, method = method))
{
    ##Method check
    if(!(method %in% c("michalsky", "cooper", "strous", "spencer"))){
        warning("'method' must be: michalsky, cooper, strous or spencer. Set michalsky")
        method = 'michalsky'
    }
    
    cosWs <- -tan(d2r(lat)) * tan(decl)
    #sunrise, negative since it is before noon
    ws <- -acos(cosWs)
    #Polar day/night
    polar <- which(is.nan(ws))
    ws[polar] <- -pi * (cosWs[polar] < -1) + 0 * (cosWs[polar] > 1)
    return(ws)
}

#### Extraterrestrial irradition ####
bo0d <- function(d, lat, method = 'michalsky',
                 decl = declination(d, method = method),
                 eo = eccentricity(d, method = method),
                 ws = sunrise(d, lat, method = method))
{
    ##Method check
    if(!(method %in% c("michalsky", "cooper", "strous", "spencer"))){
        warning("'method' must be: michalsky, cooper, strous or spencer. Set michalsky")
        method = 'michalsky'
    }

    #solar constant
    Bo <- 1367
    latr <- d2r(lat)
    #The negative sign due to the definition of ws
    Bo0d <- -24/pi * Bo * eo * (ws * sin(latr) * sin(decl) +
                                cos(latr) * cos(decl) * sin(ws))
    return(Bo0d)
}


#### Sun hour angle ####
sunHour <- function(d, BTi, sample = 'hour', EoT = TRUE,
                    method = 'michalsky',
                    eqtime = eot(d))
{
    ##Method check
    if(!(method %in% c("michalsky", "cooper", "strous", "spencer"))){
        warning("'method' must be: michalsky, cooper, strous or spencer. Set michalsky")
        method = 'michalsky'
    }

    if(missing(BTi)){
        BTi <- fBTi(BTd = d, sample = sample)
    }else {
        if (inherits(BTi, 'data.table')) {
            Times <- as.ITime(BTi$Times)
            Dates <- as.IDate(BTi$Dates)
            BTi <- as.POSIXct(Dates, Times, tz = 'UTC')
        }
        else {
            BTi <- as.POSIXct(BTi, tz = 'UTC')
        }   
    }
    rep <- cumsum(c(1, diff(as.Date(BTi)) != 0))
    if(EoT)
    {
        EoT <- eqtime
        if(length(EoT) != length(BTi)){EoT <- EoT[rep]}
    }else{EoT <- 0}

    jd <- as.numeric(julian(BTi, origin = '2000-01-01 12:00:00 UTC'))
    TO <- hms(BTi)
    
    w=switch(method,
             cooper = h2r(TO-12)+EoT,
             spencer = h2r(TO-12)+EoT,
             michalsky = {
                 meanLong <- (280.460+0.9856474*jd)%%360
                 meanAnomaly <- (357.528+0.9856003*jd)%%360
                 eclipLong <- (meanLong +1.915*sin(d2r(meanAnomaly))+0.02*sin(d2r(2*meanAnomaly)))%%360
                 excen <- 23.439-0.0000004*jd
                 
                 sinEclip <- sin(d2r(eclipLong))
                 cosEclip <- cos(d2r(eclipLong))
                 cosExcen <- cos(d2r(excen))
                 
                 ascension <- r2d(atan2(sinEclip*cosExcen, cosEclip))%%360

                 ##local mean sidereal time, LMST
                 ##TO has been previously corrected with local2Solar in order
                 ##to include the longitude, daylight savings, etc.
                 lmst <- (h2d(6.697375 + 0.0657098242*jd + TO))%%360
                 w <- (lmst-ascension)
                 w <- d2r(w + 360*(w < -180) - 360*(w > 180))
             },
             strous = {
                 meanAnomaly  <-  (357.5291 + 0.98560028*jd)%%360
                 coefC <- c(1.9148, 0.02, 0.0003)
                 sinC <- sin(outer(1:3, d2r(meanAnomaly), '*'))
                 C  <-  colSums(coefC*sinC)
                 trueAnomaly <- (meanAnomaly + C)%%360
                 eclipLong <- (trueAnomaly + 282.9372)%%360
                 excen <- 23.435
                 
                 sinEclip <- sin(d2r(eclipLong))
                 cosEclip <- cos(d2r(eclipLong))
                 cosExcen <- cos(d2r(excen))
                 
                 ascension <- r2d(atan2(sinEclip*cosExcen, cosEclip))%%360

                 ##local mean sidereal time, LMST
                 ##TO has been previously corrected with local2Solar in order
                 ##to include the longitude, daylight savings, etc.
                 lmst <- (280.1600+360.9856235*jd)%%360
                 w <- (lmst-ascension)
                 w <- d2r(w + 360*(w< -180) - 360*(w>180))
             }
             )
    return(w)
}

#### zenith angle ####
zenith <- function(d, lat, BTi, sample = 'hour',  method = 'michalsky',
                   decl = declination(d, method = method),
                   w = sunHour(d, BTi, sample, method = method))
{
    ##Method check
    if(!(method %in% c("michalsky", "cooper", "strous", "spencer"))){
        warning("'method' must be: michalsky, cooper, strous or spencer. Set michalsky")
        method = 'michalsky'
    }

    if(missing(BTi)){BTi <- fBTi(d, sample)}
    x <- as.Date(BTi)
    rep <- cumsum(c(1, diff(x) != 0))
    latr <- d2r(lat)
    if(length(decl) == length(BTi)){decl <- decl}
    else{decl <- decl[rep]}
    zenith <- sin(decl) * sin(latr) +
        cos(decl) * cos(w) * cos(latr)
    zenith <- ifelse(zenith > 1, 1, zenith)
    return(zenith)
}

#### azimuth ####
azimuth <- function(d, lat, BTi, sample = 'hour', method = 'michalsky',
                    decl = declination(d, method = method),
                    w = sunHour(d, BTi, sample, method = method),
                    cosThzS = zenith(d, lat, BTi, sample,
                                     method = method,
                                     decl = decl,
                                     w = w))
{
    ##Method check
    if(!(method %in% c("michalsky", "cooper", "strous", "spencer"))){
        warning("'method' must be: michalsky, cooper, strous or spencer. Set michalsky")
        method = 'michalsky'
    }

    signLat <- ifelse(sign(lat) == 0, 1, sign(lat)) #if the sign of lat is 0, it changes it to 1
    if(missing(BTi)){BTi <- fBTi(d, sample)}
    x <- as.Date(BTi)
    rep <- cumsum(c(1, diff(x) != 0))
    latr <- d2r(lat)
    if(length(decl) != length(BTi)){decl <- decl[rep]}
    AlS <- asin(cosThzS)
    cosazimuth <- signLat * (cos(decl) * cos(w) * sin(latr) -
                          cos(latr) * sin(decl)) / cos(AlS)
    cosazimuth <- ifelse(abs(cosazimuth)>1, sign(cosazimuth), cosazimuth)
    azimuth <- sign(w)*acos(cosazimuth)
    return(azimuth)
}
