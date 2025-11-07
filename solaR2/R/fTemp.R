fTemp<-function(sol, BD)
{
    ##sol is an object with class='Sol'
    ##BD is an object with class='Meteo', whose 'data' slot contains two columns called "TempMax" and "TempMin"

    stopifnot(class(sol) == 'Sol')
    stopifnot(class(BD) == 'Meteo')

    checkIndexD(indexD(sol), indexD(BD))

    Dates <- indexI(sol)	
    x <- as.Date(Dates)
    ind.rep <- cumsum(c(1, diff(x) != 0))
    
    TempMax <-  BD@data$TempMax[ind.rep]
    TempMin <-  BD@data$TempMin[ind.rep]
    ws <- sol@solD$ws[ind.rep]
    w <- sol@solI$w
    
    ##Generate temperature sequence from database Maxima and Minima

    Tm <- (TempMin+TempMax)/2
    Tr <- (TempMax-TempMin)/2

    wp <- pi/4

    a1 <- pi*12*(ws-w)/(21*pi+12*ws)
    a2 <- pi*(3*pi-12*w)/(3*pi-12*ws)
    a3 <- pi*(24*pi+12*(ws-w))/(21*pi+12*ws)

    T1 <- Tm-Tr*cos(a1)
    T2 <- Tm+Tr*cos(a2)
    T3 <- Tm-Tr*cos(a3)

    Ta <- T1*(w<=ws)+T2*(w>ws&w<=wp)+T3*(w>wp)

    ##Result
    result <- data.table(Dates, Ta)
}
			
