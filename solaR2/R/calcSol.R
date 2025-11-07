calcSol <- function(lat, BTd,
                    sample = 'hour', BTi,
                    EoT = TRUE,
                    keep.night = TRUE,
                    method = 'michalsky')
{
    if(missing(BTd)) BTd <- truncDay(BTi)
    solD <- fSolD(lat, BTd, method = method) #daily values
    solI <- fSolI(solD = solD, sample = sample, #intradaily values
                  BTi = BTi, keep.night = keep.night,
                  EoT = EoT, method = method)
    
    if(!missing(BTi)){
        sample <- median(diff(solI$Dates))
        sample <- format(sample)
    }
    
    solD[, lat := NULL]
    solI[, lat := NULL]
    result <- new('Sol',
                  lat = lat,
                  solD = solD,
                  solI = solI,
                  sample = sample,
                  method = method)
    return(result)
}
