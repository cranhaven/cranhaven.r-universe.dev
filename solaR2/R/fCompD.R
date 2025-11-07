utils::globalVariables('lat')

fCompD <- function(sol, G0d, corr = 'CPR', f)
{
    if(!(corr %in% c('CPR', 'Page', 'LJ', 'EKDd', 'CLIMEDd', 'user', 'none'))){
        warning('Wrong descriptor of correlation Fd-Ktd. Set CPR.')
        corr <- 'CPR'
    }
    if(class(sol)[1] != 'Sol'){
        sol <- sol[, calcSol(lat = unique(lat), BTi = Dates)]
    }
    if(class(G0d)[1] != 'Meteo'){
        dt <- copy(data.table(G0d))
        if(!('Dates' %in% names(dt))){
            dt[, Dates := indexD(sol)]
            setcolorder(dt, 'Dates')
            setkey(dt, 'Dates')
        }
        if('lat' %in% names(dt)){
            latg <- unique(dt$lat)
            dt[, lat := NULL]
        }else{latg <- getLat(sol)}
        G0d <- dt2Meteo(dt, latg)
    }  

    stopifnot(indexD(sol) == indexD(G0d))
    Bo0d <- sol@solD$Bo0d
    G0 <- getData(G0d)$G0

    is.na(G0) <- (G0>Bo0d)

    ### the Direct and Difuse data is not given
    if(corr != 'none'){
        Fd <- switch(corr,
                     CPR = FdKtCPR(sol, G0d),
                     Page = FdKtPage(sol, G0d),
                     LJ = FdKtLJ(sol, G0d),
                     EKDd = FdKtEKDd(sol, G0d),
                     CLIMEDd = FdKtCLIMEDd(sol, G0d),
                     user = f(sol, G0d))
        Kt <- Fd$Kt
        Fd <- Fd$Fd
        D0d <- Fd * G0
        B0d <- G0 - D0d
    }
    ### the Direct and Difuse data is given
    else {
        G0 <- getData(G0d)$G0d
        D0d <- getData(G0d)[['D0d']]
        B0d <- getData(G0d)[['B0d']]
        Fd <- D0d/G0
        Kt <- G0/Bo0d
    }

    result <- data.table(Dates = indexD(sol), Fd, Kt, G0d = G0, D0d, B0d)
    setkey(result, 'Dates')
    result
}
