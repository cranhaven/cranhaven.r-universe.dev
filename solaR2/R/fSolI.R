utils::globalVariables(c('eqtime', 'w', 'night', 'cosThzS',
                         'AlS', 'AzS', 'Times'))

fSolI <- function(solD, sample = 'hour', BTi,
                  EoT = TRUE, keep.night = TRUE, method = 'michalsky')
{
    #Solar constant
    Bo <- 1367

    if(missing(BTi)){
        BTd <- solD$Dates
        BTi <- fBTi(BTd, sample)
    }
    sun <- data.table(Dates = as.IDate(BTi),
                      Times = as.ITime(BTi))
    sun <- merge(solD, sun, by = 'Dates')
    sun[, eqtime := EoT]
    sun[, EoT := NULL]

    #sun hour angle
    sun[, w := sunHour(Dates, BTi, EoT = EoT, method = method, eqtime = eqtime)]

    #classify night elements
    sun[, night := abs(w) >= abs(ws)]
    
    #zenith angle
    sun[, cosThzS := zenith(Dates, lat, BTi,
                            method = method,
                            decl = decl,
                            w = w
                            )]

    #solar altitude angle
    sun[, AlS := asin(cosThzS)]
    
    #azimuth
    sun[, AzS := azimuth(Dates, lat, BTi, sample,
                         method = method,
                         decl = decl, 
                         w = w,
                         cosThzS = cosThzS)]

    #Extraterrestrial irradiance
    sun[, Bo0 := Bo * eo * cosThzS]
    
    #When it is night there is no irradiance
    sun[night == TRUE, Bo0 := 0]

    #Erase columns that are in solD
    sun[, decl := NULL]
    sun[, eo := NULL]
    sun[, eqtime := NULL]
    sun[, ws := NULL]
    sun[, Bo0d := NULL]

    #Column Dates with Times
    sun[, Dates := as.POSIXct(Dates, Times, tz = 'UTC')]
    sun[, Times := NULL]
    
    #keep night
    if(!keep.night){
        sun <- sun[night == FALSE]
    }

    return(sun)
}
