reapmulthc <- function(mu, RES, lam, vres, cvari, nodos1, nfac, display){

  #begin timer
  #ptm <- proc.time()

  ptm_one <- proc.time()

  n<-size(RES)[1]

  for (i in 1:n){

    if (nfac==2){
      OUT <- eapmulthc2(mu, RES[i,], lam, vres, cvari, nodos1)
      tmp1<-OUT$th
      tmp2<-OUT$st
    }
    if (nfac==3){
      OUT <- eapmulthc3(mu, RES[i,], lam, vres, cvari, nodos1)
      tmp1<-OUT$th
      tmp2<-OUT$st
    }
    if (nfac==4){
      OUT <- eapmulthc4(mu, RES[i,], lam, vres, cvari, nodos1)
      tmp1<-OUT$th
      tmp2<-OUT$st
    }

    if (i == 1){
      FAC <- c(tmp1,tmp2)
    }
    else {
      FAC<-rbind(FAC,c(tmp1,tmp2))
    }


    if (display==TRUE){

      compT <- proc.time() - ptm_one
      compT<-compT[3]
      compT<-compT*(n-i)/i

      secondsInAMinute = 60
      secondsInAnHour = 60 * secondsInAMinute
      secondsInADay = 24 * secondsInAnHour

      days <- floor(compT / secondsInADay)

      hourSeconds <- compT %% secondsInADay
      hours <- floor(hourSeconds / secondsInAnHour)

      minuteSeconds <- hourSeconds %% secondsInAnHour
      minutes <- floor(minuteSeconds / secondsInAMinute)

      remainingSeconds <- minuteSeconds %% secondsInAMinute
      seconds <- ceiling(remainingSeconds)

      if (compT > 3600){
        if (days >= 1){ #Very very rare, but just to be sure
          cat("Computing appropiateness. Time remaining: +24 hours                                                     \r")
          flush.console()
        }
        else {
          cat("Computing appropiateness. Time remaining: ", hours,"hours, ",minutes, "minutes and ",seconds, "seconds \r")
          flush.console()
        }
      }
      else{
        if (compT >= 60){
          cat("Computing appropiateness. Time remaining: ", minutes, "minutes and ",seconds,"seconds \r")
          flush.console()
        }
        if (compT < 60) {
          cat("Computing appropiateness. Time remaining ",seconds,"seconds                                                                  \r")
          flush.console()
        }
      }
    }

  }


    if (display==TRUE){
      cat("\r","                                                                                                  ","\r")


    }



  return(FAC)



}
