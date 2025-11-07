intervalo <- function(day, sample){
    intervalo <- seq.POSIXt(from = as.POSIXct(paste(day, '00:00:00'), tz = 'UTC'),
                      to = as.POSIXct(paste(day, '23:59:59'), tz = 'UTC'),
                      by = sample)
    return(intervalo)
}

fBTi <- function(BTd, sample = 'hour'){
    BTi <- lapply(BTd, intervalo, sample)
    BTi <- do.call(c, BTi)
    return(BTi)
}

