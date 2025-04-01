reapmulc <- function(mu, RES, lam, vres, nodos1, nodos2, nfac, display){

  #begin timer
  #ptm <- proc.time()

  ptm_one <- proc.time()

  n<-size(RES)[1]

  for (i in 1:n){

    if (nfac==2){
      OUT <- eapmulc2(mu, RES[i,], lam, vres, nodos1, nodos2)
      tmp1<-OUT$th
      tmp2<-OUT$vi
      tmp3<-OUT$st
      tmp4<-OUT$sv
      tmp5<-OUT$reli_th
      tmp6<-OUT$reli_PDD
    }
    if (nfac==3){
      OUT <- eapmulc3(mu, RES[i,], lam, vres, nodos1, nodos2)
      tmp1<-OUT$th
      tmp2<-OUT$vi
      tmp3<-OUT$st
      tmp4<-OUT$sv
      tmp5<-OUT$reli_th
      tmp6<-OUT$reli_PDD
    }
    if (nfac==4){
      OUT <- eapmulc4(mu, RES[i,], lam, vres, nodos1, nodos2)
      tmp1<-OUT$th
      tmp2<-OUT$vi
      tmp3<-OUT$st
      tmp4<-OUT$sv
      tmp5<-OUT$reli_th
      tmp6<-OUT$reli_PDD
    }

    if (i == 1){
      FAC <- c(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6)
    }
    else {
      FAC<-rbind(FAC,c(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6))
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
          cat("Computing InDisc. Time remaining: +24 hours                                                     \r")
          flush.console()
        }
        else {
          cat("Computing InDisc. Time remaining: ", hours,"hours, ",minutes, "minutes and ",seconds, "seconds \r")
          flush.console()
        }
      }
      else{
        if (compT >= 60){
          cat("Computing InDisc. Time remaining: ", minutes, "minutes and ",seconds,"seconds \r")
          flush.console()
        }
        if (compT < 60) {
          cat("Computing InDisc. Time remaining ",seconds,"seconds                                                                  \r")
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
