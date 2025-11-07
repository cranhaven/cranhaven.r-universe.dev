optimShd<-function(lat,
                   modeTrk = 'fixed', 
                   modeRad = 'prom', 
                   dataRad,
                   sample = 'hour',
                   keep.night = TRUE,
                   sunGeometry = 'michalsky',
                   betaLim = 90, beta = abs(lat)-10, alpha = 0,
                   iS = 2, alb = 0.2, HCPV = FALSE,
                   module = list(), 
                   generator = list(),
                   inverter = list(), 
                   effSys = list(), 
                   modeShd = '',    
                   struct = list(), 
                   distances = data.table(),
                   res = 2,      #resolution, distance spacing
                   prog = TRUE){ #Drawing progress bar
		
    if (('bt' %in% modeShd) & (modeTrk!='horiz')) {
        modeShd[which(modeShd=='bt')] = 'area'
        warning('backtracking is only implemented for modeTrk = horiz')}

    ##I save function arguments for later use

    listArgs <- list(lat = lat, modeTrk = modeTrk, modeRad = modeRad,
                     dataRad = dataRad,
                     sample = sample, keep.night = keep.night,
                     sunGeometry = sunGeometry,
                     betaLim = betaLim, beta = beta, alpha = alpha,
                     iS = iS, alb = alb, HCPV = HCPV,
                     module = module, generator = generator,
                     inverter = inverter, effSys = effSys,
                     modeShd = modeShd, struct = struct,
                     distances = data.table(Lew = NA, Lns = NA, D = NA))
    
    
    ##Create network on which I will do the calculations
    Red <- switch(modeTrk,
                  horiz = with(distances,
                             data.table(Lew = seq(Lew[1],Lew[2],by = res),
                                        H = 0)),
                  two = with(distances,
                           data.table(
                               expand.grid(Lew = seq(Lew[1],Lew[2],by = res),
                                           Lns = seq(Lns[1],Lns[2],by = res),
                                           H = 0))),
                  fixed = with(distances,
                             data.table(D = seq(D[1],D[2],by = res),
                                        H = 0))
                  )
    
    casos <- dim(Red)[1] #Number of possibilities to study

    ##I prepare the progress bar
    if (prog) {pb <- txtProgressBar(min = 0, max = casos+1, style = 3)
        setTxtProgressBar(pb, 0)}
    
###Calculations	
    ##Reference: No shadows	
    listArgs0 <- modifyList(listArgs,
                            list(modeShd = '', struct = NULL, distances = NULL) )
    Prod0 <- do.call(prodGCPV, listArgs0)
    YfAnual0 <- mean(Prod0@prody$Yf) #I use mean in case there are several years
    if (prog) {setTxtProgressBar(pb, 1)}
    
    ##The loop begins
    
    ##I create an empty vector of the same length as the cases to be studied
    YfAnual <- numeric(casos) 
    
    BT <- ('bt' %in% modeShd)
    if (BT) { ##There is backtracking, then I must start from horizontal radiation.
        RadBT <- as(Prod0, 'G0')
        for (i in seq_len(casos)){
            listArgsBT <- modifyList(listArgs,
                                     list(modeRad = 'prev', dataRad = RadBT,
                                          distances = Red[i,]))
            prod.i <- do.call(prodGCPV, listArgsBT)
            YfAnual[i] <- mean(prod.i@prody$Yf)
            if (prog) {setTxtProgressBar(pb, i+1)}
        }
    } else {
        prom <- ('prom' %in% modeShd)
        for (i in seq_len(casos)){
            Gef0 <- as(Prod0, 'Gef')
            GefShd <- calcShd(Gef0, modeShd = modeShd,
                           struct = struct, distances = Red[i,])
            listArgsShd <- modifyList(listArgs,
                                      list(modeRad = 'prev', dataRad = GefShd)
                                      )
            prod.i <- do.call(prodGCPV, listArgsShd)
            YfAnual[i] <- mean(prod.i@prody$Yf)
            if (prog) {setTxtProgressBar(pb, i+1)}
        }
    }
    if (prog) {close(pb)}
    
    
###Results
    FS <- 1-YfAnual/YfAnual0
    GRR <- switch(modeTrk,
                  two = with(Red,Lew*Lns)/with(struct,L*W),
                  fixed = Red$D/struct$L,
                  horiz = Red$Lew/struct$L)
    SombraDF <- data.table(Red,GRR,FS,Yf = YfAnual)
    FS.loess <- switch(modeTrk,
                       two = loess(FS~Lew*Lns,data = SombraDF),
                       horiz = loess(FS~Lew,data = SombraDF),
                       fixed = loess(FS~D,data = SombraDF))
    Yf.loess <- switch(modeTrk,
                       two = loess(Yf~Lew*Lns,data = SombraDF),
                       horiz = loess(Yf~Lew,data = SombraDF),
                       fixed = loess(Yf~D,data = SombraDF))
    result <- new('Shade',
                  Prod0, ##contains ProdGCPV
                  FS = FS,
                  GRR = GRR,
                  Yf = YfAnual,
                  FS.loess = FS.loess,
                  Yf.loess = Yf.loess,
                  modeShd = modeShd,
                  struct = struct,
                  distances = Red,
                  res = res
                  )
    result
}
