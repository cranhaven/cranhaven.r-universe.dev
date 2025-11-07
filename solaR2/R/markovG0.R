## Objects loaded at startup from data/MTM.RData
if(getRversion() >= "2.15.1") globalVariables(c(
                  'MTM', ## Markov Transition Matrices
                  'Ktmtm', ## Kt limits to choose a matrix from MTM
                  'Ktlim' ## Daily kt range of each matrix.
                  ))
                  
markovG0 <- function(G0dm, solD){
    solD <- copy(solD)
    timeIndex <- solD$Dates
    Bo0d <- solD$Bo0d
    Bo0dm <- solD[, mean(Bo0d), by = .(month(Dates), year(Dates))][[3]]
    ktm <- G0dm/Bo0dm
    
    ##Calculates which matrix to work with for each month
    whichMatrix <- findInterval(ktm, Ktmtm, all.inside = TRUE)
    
    ktd <- state <- numeric(length(timeIndex))
    state[1] <- 1
    ktd[1] <- ktm[state[1]]
    for (i in 2:length(timeIndex)){
        iMonth <- month(timeIndex[i])
        colMonth <- whichMatrix[iMonth]
        rng <- Ktlim[, colMonth]
        classes <- seq(rng[1], rng[2], length=11)
        matMonth <- MTM[(10*colMonth-9):(10*colMonth),]
        ## http://www-rohan.sdsu.edu/~babailey/stat575/mcsim.r
        state[i] <- sample(1:10, size=1, prob=matMonth[state[i-1],])
        ktd[i] <- runif(1, min=classes[state[i]], max=classes[state[i]+1])
    }
    G0dmMarkov <- data.table(ktd, Bo0d)
    G0dmMarkov <- G0dmMarkov[, mean(ktd*Bo0d), by = .(month(timeIndex), year(timeIndex))][[3]]
    fix <- G0dm/G0dmMarkov
    indRep <- month(timeIndex)
    fix <- fix[indRep]
    G0d <- data.table(Dates = timeIndex, G0d = ktd * Bo0d * fix)
    G0d
}
