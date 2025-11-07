utils::globalVariables(c('decl', 'eo', 'EoT', 'ws'))

fSolD <- function(lat, BTd, method = 'michalsky'){
    if (abs(lat) > 90){
        lat <- sign(lat) * 90
        warning(paste('Latitude outside acceptable values. Set to', lat))
    }
    sun <- data.table(Dates = unique(as.IDate(BTd)),
                      lat = lat)
    
    #### solarAngles ####
    
    ##Declination
    sun[, decl := declination(Dates, method = method)]
    ##Eccentricity
    sun[, eo := eccentricity(Dates, method = method)]
    ##Equation of time
    sun[, EoT := eot(Dates)]
    ##Solar time
    sun[, ws := sunrise(Dates, lat, method = method,
                        decl = decl)]
    ##Extraterrestrial irradiance
    sun[, Bo0d := bo0d(Dates, lat, method = method,
                       decl = decl,
                       eo = eo,
                       ws = ws
                       )]
    setkey(sun, Dates)
    sun
}
