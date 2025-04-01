reap_grad_obli<-function(X, LAM, PHI, THRES, sigj,disp){

  m<-size(LAM)[1]
  r<-size(LAM)[2]

  #7 nodes
  grid<-transpose(seq(-4,4,(8/7)))

  # Number of nodes, for now 7
  ni<-7
  tmp<-size(grid)[2]
  index_nodos <- Nseriesk(ni,r)

  #########
  n<-size(X)[1]
  m<-size(X)[2]

  th<-numeric()
  se<-numeric()
  reli<-numeric()
  incons <- numeric()
  #X<-as.numeric(X)

  ptm_one <- proc.time()

  for (i in 1:n){

    #if (disp==TRUE){

      #if (i==1){
        #calculate the time elapsed for computing the first one for estimating the total elapsed time
      #  ptm_one <- proc.time()
      #}

      #if (i==2){
        #waitbar
        #cat('Computing EAP scores: Please wait \n')
        #pb <- txtProgressBar(min = 0, max = n-1, style = 3)
      #}
    #}

    if (min(X)==0){
      X=X+1
    }

    out <- eap_grad_obli(X[i,],LAM,PHI,THRES,sigj,grid,index_nodos)

    th <- rbind(th,t(out$th_i))
    se <- rbind(se,t(out$se_i))
    reli <- rbind(reli,t(out$reli_i))
    incons <- rbind(incons,t(as.numeric(out$reli_i < 0.10)))
    #if (any(out$reli_i < 0.10)){ incons <- rbind(incons, 0)} else {incons <- rbind(incons,1)} #if any value <0.10

    if (disp==TRUE){

     # if (i==1){
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
            cat("Computing EAP scores Time remaining: +24 hours                                                       \r")
            flush.console()
          }
          else {
            if (hours == 1){
              cat("Computing EAP scores. Time remaining: ", hours,"hour, ",minutes, "minutes and ",seconds, "seconds  \r")
              flush.console()
            }
            else {
              cat("Computing EAP scores. Time remaining: ", hours,"hours, ",minutes, "minutes and ",seconds, "seconds \r")
              flush.console()
            }
          }
        }
        else{
          if (compT >= 60){
            cat("Computing EAP scores. Time remaining: ", minutes, "minutes and ",seconds,"seconds \r")
            flush.console()
          }
          if (compT < 60) {
            cat("Computing EAP scores. Time remaining ",seconds,"seconds                                                                  \r")
            flush.console()
          }
        }

        #Sys.sleep(0.01)

        #if (et.minutes<=1){
        #  cat('Estimated time for the analysis: less than a minute')
        #}
        #if (et.minutes>1 && et.minutes<=1.5) {
        #  cat(sprintf('Estimated time for the analysis: %3.0f minute',round(et.minutes)))
        #}
        #if (et.minutes>1.5) {
        #  cat(sprintf('Estimated time for the analysis: %3.0f minutes',round(et.minutes)))
        #}

        #cat('\n\n')
      #}
      #else{
        #Sys.sleep(0.1)
        # update progress bar
        #setTxtProgressBar(pb, i-1)
      #}
    }
  }

  if (disp==TRUE){
    cat("\r","                                                                                                  ","\r")
    #close(pb)
  }

  z90<-1.64485362695147

  th_li<-matrix(0,n,r)
  th_ls<-matrix(0,n,r)
  for (i in 1:n){
    for (j in 1:r){
      th_li[i,j] <- th[i,j] - z90*se[i,j]
      th_ls[i,j] <- th[i,j] + z90*se[i,j]
    }
  }

  OUT<-list("th"=th, "th_li"=th_li, "th_ls"=th_ls,"se"=se,"reli"=reli,"incons" = incons)
  return(OUT)

}
