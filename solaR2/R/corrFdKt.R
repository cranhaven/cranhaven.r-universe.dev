utils::globalVariables(c('G0', 'lag1', 'lag2'))

#### monthly Kt ####
Ktm <- function(sol, G0dm){
    solf <- sol@solD[, .(Dates, Bo0d)]
    solf[, c('month', 'year') := .(month(Dates), year(Dates))]
    solf[,Bo0m := mean(Bo0d), by = .(month, year)]
    G0df <- G0dm@data[, .(Dates, G0d)]
    G0df[, c('month', 'year') := .(month(Dates), year(Dates))]
    G0df[, G0d := mean(G0d), by = .(month, year)]
    Ktm <- G0df$G0d/solf$Bo0m
    return(Ktm)
}

#### daily Kt ####
Ktd <- function(sol, G0d){
    Bo0d <- sol@solD$Bo0d
    G0d <- getG0(G0d)
    Ktd <- G0d/Bo0d
    return(Ktd)
}

### intradaily
Kti <- function(sol, G0i){
    Bo0 <- sol@solI$Bo0
    G0i <- getG0(G0i)
    Kti <- G0i/Bo0
    return(Kti)
}


#### monthly correlations ####

### Page ###
FdKtPage <- function(sol, G0dm){
    Kt <- Ktm(sol, G0dm)
    Fd = 1-1.13*Kt
    return(data.table(Fd, Kt))
}

### Liu and Jordan ###
FdKtLJ <- function(sol, G0dm){
    Kt <- Ktm(sol, G0dm)
    Fd = (Kt<0.3)*0.595774 +
        (Kt>=0.3 & Kt<=0.7)*(1.39-4.027*Kt+5.531*Kt^2-3.108*Kt^3)+
        (Kt>0.7)*0.215246
    return(data.table(Fd, Kt))
}


#### daily correlations ####

### Collares-Pereira and Rabl
FdKtCPR <- function(sol, G0d){
    Kt <- Ktd(sol, G0d)
    Fd = (0.99*(Kt<=0.17))+(Kt>0.17 & Kt<0.8)*
        (1.188-2.272*Kt+9.473*Kt^2-21.856*Kt^3+14.648*Kt^4)+
        (Kt>=0.8)*0.2426688      
    return(data.table(Fd, Kt))
}

### Erbs, Klein and Duffie ###
FdKtEKDd <- function(sol, G0d){
    ws <- sol@solD$ws
    Kt <- Ktd(sol, G0d)
      
    WS1 = (abs(ws)<1.4208)
    Fd = WS1*((Kt<0.715)*(1-0.2727*Kt+2.4495*Kt^2-11.9514*Kt^3+9.3879*Kt^4)+
            (Kt>=0.715)*(0.143))+
        !WS1*((Kt<0.722)*(1+0.2832*Kt-2.5557*Kt^2+0.8448*Kt^3)+
              (Kt>=0.722)*(0.175))
  return(data.table(Fd, Kt))
}

### CLIMED1 ###
FdKtCLIMEDd <- function(sol, G0d){
    Kt <- Ktd(sol, G0d)
    Fd = (Kt<=0.13)*(0.952)+
    (Kt>0.13 & Kt<=0.8)*(0.868+1.335*Kt-5.782*Kt^2+3.721*Kt^3)+
      (Kt>0.8)*0.141
  return(data.table(Fd, Kt))
}

#### intradaily correlations ####

### intradaily EKD ###
FdKtEKDh <- function(sol, G0i){
    Kt <- Kti(sol, G0i)
    Fd = (Kt<=0.22)*(1-0.09*Kt)+
    (Kt>0.22 & Kt<=0.8)*(0.9511-0.1604*Kt+4.388*Kt^2-16.638*Kt^3+12.336*Kt^4)+
      (Kt>0.8)*0.165
  return(data.table(Fd, Kt))
}

### intradaily CLIMED
FdKtCLIMEDh <- function(sol, G0i){
    Kt <- Kti(sol, G0i)
    Fd = (Kt<=0.21)*(0.995-0.081*Kt)+
        (Kt>0.21 & Kt<=0.76)*(0.724+2.738*Kt-8.32*Kt^2+4.967*Kt^3)+
        (Kt>0.76)*0.180
    return(data.table(Fd, Kt))
}

### intradaily Boland, Ridley and Lauret ###
FdKtBRL <- function(sol, G0i){
    Kt <- Kti(sol, G0i)
    sample <- sol@sample
    ind <- indexI(sol)
    
    solI <- as.data.tableI(sol, complete = TRUE)
    w <- solI$w
    night <- solI$night
    AlS <- solI$AlS
    Bo0 <- solI$Bo0

    G0d <- data.table(ind,
                      G0 = getG0(G0i),
                      Bo0 = Bo0)
    G0d[, G0d := P2E(G0, sample), by = truncDay(ind)]
    G0d[, Bo0d := P2E(Bo0, sample), by = truncDay(ind)]
    ktd <- G0d[, ifelse(night, 0, G0d/Bo0d)]
    
    ##persistence    
    pers <- persistence(sol, Kt)

    
    ##fd calculation
    Fd = (1+exp(-5.38+6.63*Kt+0.006*r2h(w)-0.007*r2d(AlS)+1.75*ktd+1.31*pers))^(-1)
    
    return(data.table(Fd, Kt))
}

persistence <- function(sol, kt){
    kt <- data.table(ind = indexI(sol), kt)
    ktNA <- na.omit(copy(kt))
    iDay <- truncDay(ktNA[[1]])

    x <- rle(as.numeric(iDay))$lengths
    xLast <- cumsum(x)
    xFirst <- xLast-x+1

    ktNA[, lag1 := shift(kt, n = -1, type = 'lag', fill = NA)]
    ktNA[xLast, lag1 := ktNA[xLast-1, kt]]
    
    ktNA[, lag2 := shift(kt, n = 1, type = 'lag', fill = NA)]
    ktNA[xFirst, lag2 := ktNA[xFirst + 1, kt]]
    
    pers <- merge(kt, ktNA, by = 'ind', all = TRUE)
    return(pers[, 1/2*(lag1+lag2)])
}
