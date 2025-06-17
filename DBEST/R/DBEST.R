DBEST <-
function(data, data.type, seasonality=-1, algorithm,  breakpoints.no=-1, generalization.percent=-1, change.magnitude=-1, first.level.shift, second.level.shift, duration, distance.threshold, alpha, plot=-1){

        data.type <- tolower(data.type)
        algorithm <- tolower(algorithm)
        
        if(is.character(distance.threshold)) {
                distance.threshold <- tolower(distance.threshold)
        }
        
        
        if(is.character(breakpoints.no)) {
                breakpoints.no <- tolower(breakpoints.no)
                if(breakpoints.no=="all") {
                        breakpoints.no <- length(as.vector(data))
                }
        }
        
        
        if(data.type=="non-cyclical") {
                data.type <- "noncyclical"
        }
        
        if(data.type=="non cyclical") {
                data.type <- "noncyclical"
        }
        
        if(algorithm=="generalization") {
                algorithm <- "generalisation"
        }
        
        if(algorithm=="change detection") {
                algorithm <- "changedetection"
        }
        
        if(algorithm=="change-detection") {
                algorithm <- "changedetection"
        }
        
        if((breakpoints.no)!=-1) {
                option <- "breakpoint"
        }
        
        if(generalization.percent!=-1) {
                option <- "percent"
        }
        
        if(change.magnitude!=-1) {
                option <- "magnitude"
        }

        if(seasonality!=-1) {
                
                if(is.character(seasonality)) {
                        seasonality <- tolower(seasonality)
                }
                
                if(is.zoo(data) || is.ts(data)) {
                        
                        if(seasonality=="none" || seasonality=="null") {
                                data <- as.zoo(ts(as.vector(data), start=time(data)[1]))
                        } else {
                                data <- as.zoo(ts(as.vector(data),  start = time(data)[1], frequency=seasonality))  
                        }

                } else {
                        
                        if(seasonality=="none" || seasonality=="null") {
                                data <- as.zoo(ts(as.vector(data), start=1980, deltat=1/12))
                        } else {
                                data <- as.zoo(ts(as.vector(data), start=1980, frequency=seasonality))
                        }

                }
                
        }

        
        flvl <- f_level_shift(as.vector(data),first.level.shift,second.level.shift,duration)
        vec <- flvl$LS_point
        no <- flvl$no
        
        npoints <- length(as.vector(data))
        
        switch(data.type,
               cyclical = {
                       
                       if(no > 0) {
                               
                               initial <- 1
                               trend <- c()
                               seasonal <- c()
                               remainder <- c()
                               temp <- sort(vec)
                               
                               for(i in 1:no) {
                                       
                                       #stl.tmp <- stl(data[initial:temp[i]], s.window = "per") ## s.window = 12
                                       stl.tmp <- stl(data[initial:temp[i]], s.window = 7)
                                       trend1 <- stl.tmp$time.series[,"trend"]
                                       seasonal1 <- stl.tmp$time.series[,"seasonal"]
                                       remainder1  <- stl.tmp$time.series[,"remainder"]
                                       
                                       trend <- c(trend, trend1)
                                       seasonal <- c(seasonal, seasonal1)
                                       remainder <- c(remainder, remainder1)
                                       
                                       initial <- temp[i] + 1  
                               } ## end for loop
                               
                               #stl.tmp <- stl(data[initial:npoints], s.window = "per")
                               stl.tmp <- stl(data[initial:npoints], s.window = 7)
                               trend2 <- stl.tmp$time.series[,"trend"]
                               seasonal2 <- stl.tmp$time.series[,"seasonal"]
                               remainder2  <- stl.tmp$time.series[,"remainder"]
                               
                               trend <- c(trend, trend2)
                               seasonal <- c(seasonal, seasonal2)
                               remainder <- c(remainder, remainder2)
                               
                               trend <- as.vector(trend)
                               
                       } else { ## end if no > 0
                               
                               #stl.tmp <- stl(data, s.window = "per")
                               stl.tmp <- stl(data, s.window = 7)
                               trend <- stl.tmp$time.series[,"trend"]
                               seasonal <- stl.tmp$time.series[,"seasonal"]
                               remainder  <- stl.tmp$time.series[,"remainder"]
                               
                               trend <- as.vector(trend)
                               
                       } ## end else  no > 0
                       
               }, 
               noncyclical = {
                       trend <- as.vector(data)
               }
               
        ) ## end swtich 'type'
        
        if(distance.threshold == "default") {
                epsilon_max <- abs(max(as.vector(data)) - min(as.vector(data)))
                distance.threshold <- Compute_epsilon(trend,epsilon_max,vec)
        }
        
        
        
        TLC <- Trend_local_change(trend,distance.threshold,vec)
        
        turning_no <- TLC$n_turning
        f_local <- TLC$f_local
        f_sign <- TLC$f_sign
        turning <- TLC$Turning
        RL_DIST <- TLC$RL_DIST
        
        
        BIC_BP_no <- BIC_BP(trend,f_local,turning_no,f_sign,turning)
        
        DTSBP <- DBEST_timeseries_BP(trend,BIC_BP_no,f_local,turning_no,f_sign,turning)
        Start <- DTSBP$Start
        End <- DTSBP$End
        Duration <- DTSBP$Duration
        Magnitude <- DTSBP$Magnitude
        BP_mark <- DTSBP$BP_mark
        BP_order <- DTSBP$BP_order
        trend_data_series <- DTSBP$trend_data_series
        Param_no <- DTSBP$Param_no
        RSS <- DTSBP$RSS
        RMSE <- DTSBP$RMSE
        MAD <- DTSBP$MAD
        p <- DTSBP$p
        R <- DTSBP$R
        dfSJ <- DTSBP$dfSJ
        normr <- DTSBP$normr
        shared <- DTSBP$shared
        
        Pvalue <- c()
        
        for(i in 1:(Param_no-1)) {
                Pvalue <- c(Pvalue, Pvalue_ith_coeff(p,R,normr,dfSJ,i))
        }
        
        Sig_Slope_no <- length(Pvalue[Pvalue[-length(Pvalue)] < alpha]) - 1
        Sig_Slope_position <- c()
        Sig_BP_position <- c()
        
        if(Sig_Slope_no>0) {
                Sig_Slope_position <- which(Pvalue[-length(Pvalue)]<alpha)
                Sig_turning_position <- c()
                if(shared[1] == 1) {
                        for(i in 1:Sig_Slope_no) {
                                Sig_turning_position[i] <- shared[Sig_Slope_position[i]]
                        }
                } else {
                        for(i in 1:Sig_Slope_no) {
                                if(Sig_Slope_position[i]==1) {
                                        Sig_turning_position[i] <- 1
                                } else {
                                        Sig_turning_position[i] <- shared[Sig_Slope_position[i]-1]
                                }
                        }
                }
                
                Sig_BP_position <- intersect(Start,Sig_turning_position)
        }
        
        
        switch(algorithm, 
               generalisation={
                       
                       switch(option, 
                              percent={
                                      
                                      BP_no <- ceiling( BIC_BP_no - ( BIC_BP_no * ( as.integer(generalization.percent)/100)))
                                      DTSBP <- DBEST_timeseries_BP(trend,BP_no,f_local,turning_no,f_sign,turning)
                                      Start <- DTSBP$Start
                                      End <- DTSBP$End
                                      Duration <- DTSBP$Duration
                                      Magnitude <- DTSBP$Magnitude
                                      BP_mark <- DTSBP$BP_mark
                                      BP_order <- DTSBP$BP_order
                                      fit <- DTSBP$trend_data_series
                                      Param_no <- DTSBP$Param_no
                                      RSS <- DTSBP$RSS
                                      RMSE <- DTSBP$RMSE
                                      MAD <- DTSBP$MAD
                                      p <- DTSBP$p
                                      R <- DTSBP$R
                                      dfSJ <- DTSBP$dfSJ
                                      normr <- DTSBP$normr
                                      shared <- DTSBP$shared
                              },
                              breakpoint={
                                      if(breakpoints.no>BIC_BP_no) {
                                              breakpoints.no <- BIC_BP_no
                                      }
                                      BP_no <- breakpoints.no
                                      
                                      DTSBP <- DBEST_timeseries_BP(trend,BP_no,f_local,turning_no,f_sign,turning)
                                      
                                      Start <- DTSBP$Start
                                      End <- DTSBP$End
                                      Duration <- DTSBP$Duration
                                      Magnitude <- DTSBP$Magnitude
                                      BP_mark <- DTSBP$BP_mark
                                      BP_order <- DTSBP$BP_order
                                      fit <- DTSBP$trend_data_series
                                      Param_no <- DTSBP$Param_no
                                      RSS <- DTSBP$RSS
                                      RMSE <- DTSBP$RMSE
                                      MAD <- DTSBP$MAD
                                      p <- DTSBP$p
                                      R <- DTSBP$R
                                      dfSJ <- DTSBP$dfSJ
                                      normr <- DTSBP$normr
                                      shared <- DTSBP$shared
                                      
                              },
                              magnitude={
                                      
                                      DTSTH <- DBEST_timeseries_Threshold(trend,change.magnitude,f_local,turning_no,f_sign,turning,BIC_BP_no)
                                      
                                      BP_no <- DTSTH$n_BP
                                      Start <- DTSTH$Start
                                      End <- DTSTH$End
                                      Duration <- DTSTH$Duration
                                      Magnitude <- DTSTH$Magnitude
                                      BP_mark <- DTSTH$BP_mark
                                      BP_order <- DTSTH$BP_order
                                      fit <- DTSTH$trend_data_series
                                      Param_no <- DTSTH$Param_no
                                      RSS <- DTSTH$RSS
                                      RMSE <- DTSTH$RMSE
                                      MAD <- DTSTH$MAD
                                      shared <- DTSTH$shared
                                      
                              }
                              
                       ) ## end switch option
                       
                       segment_no <- Param_no-1
                       BP_no <- BP_no
                       
                       Output.SegmentNo <- segment_no
                       Output.RMSE <- RMSE
                       Output.MAD <- MAD
                       Output.Fit <- fit
                       
                       DBEST.values <- list(
                               "SegmentNo" = Output.SegmentNo,
                               "RMSE" = Output.RMSE,
                               "MAD" = Output.MAD,
                               "Fit" = Output.Fit
                       )
                       
               }, ## end case generalisation algorithm
               changedetection={
                       
                       
                       DTSBP <- DBEST_timeseries_BP(trend,BIC_BP_no,f_local,turning_no,f_sign,turning)
                       Start <- DTSBP$Start
                       End <- DTSBP$End
                       Duration <- DTSBP$Duration
                       Magnitude <- DTSBP$Magnitude
                       BP_mark <- DTSBP$BP_mark
                       BP_order <- DTSBP$BP_order
                       fit <- DTSBP$trend_data_series
                       Param_no <- DTSBP$Param_no
                       RSS <- DTSBP$RSS
                       RMSE <- DTSBP$RMSE
                       MAD <- DTSBP$MAD
                       shared <- DTSBP$shared
                       
                       switch(option, 
                              breakpoint={
                                      if(breakpoints.no>BIC_BP_no) {
                                              breakpoints.no<-BIC_BP_no
                                      }
                              },
                              magnitude={
                                      breakpoints.no <- length(Magnitude[abs(Magnitude)>change.magnitude])
                                      
                              }
                       ) ## end switch option
                       
                       
                       segment_no <- Param_no-1
                       BP_no <- breakpoints.no
                       
                       Output.BreakpointNo <- breakpoints.no
                       Output.SegmentNo <- segment_no
                       Output.Fit <- fit
                       
                       if(breakpoints.no == 0) {
                               
                               Output.Start <- c()
                               Output.Duration <- c()
                               Output.End <- c()
                               Output.Change <- c()
                               Output.ChangeType <- c()
                               Output.Significance <- c()
   
                       } else {

                               Output.Start <- Start[1:breakpoints.no]
                               Output.Duration <- Duration[1:breakpoints.no]
                               Output.End <- End[1:breakpoints.no]
                               Output.Change <- Magnitude[1:breakpoints.no]
                               Output.ChangeType <- match(Start[1:breakpoints.no],vec)
                               Output.ChangeType[!is.na(Output.ChangeType)] <- 1 ## matched to 1
                               Output.ChangeType[is.na(Output.ChangeType)] <- 0 ## NA to 0
                               Output.Significance= match(Start[1:breakpoints.no],Sig_BP_position);
                               Output.Significance[!is.na(Output.Significance)] <- 1 ## matched to 1
                               Output.Significance[is.na(Output.Significance)] <- 0 ## NA to 0
                       }

                       
                       
                       DBEST.values <- list(
                               "BreakpointNo" = Output.BreakpointNo,
                               "SegmentNo" = Output.SegmentNo,
                               "Start" = Output.Start,
                               "Duration" = Output.Duration,
                               "End" = Output.End,
                               "Change" = Output.Change,
                               "ChangeType" = Output.ChangeType,
                               "Significance" = Output.Significance,
                               "Fit" = Output.Fit
                       )
                       
                       
               }
               
        ) ## end switch algorithm
        
        
        DBEST.values <- append(DBEST.values, list(Data=data))
        
        
        if(exists("trend")) {
                DBEST.values <- append(DBEST.values, list(Trend=trend))
        }
        
        if(exists("seasonal")) {
                DBEST.values <- append(DBEST.values, list(Seasonal=seasonal))
        }
        
        if(exists("remainder")) {
                DBEST.values <- append(DBEST.values, list(Remainder=remainder))
        }
        
        if(exists("f_local")) {
                DBEST.values <- append(DBEST.values, list(f_local=f_local))
                
        }
        
        class(DBEST.values) <- "DBEST"
        
        if(plot!=-1) {
                if(is.character(plot)) {
                        plot <- tolower(plot)
                }
                        
                if(plot=="on") {
                        plot.DBEST(DBEST.values, figure=1)                        
                        plot.DBEST(DBEST.values, figure=2)
                }
                
                if(plot=="fig1") {
                        plot.DBEST(DBEST.values, figure=1)                        
                }
                
                if(plot=="fig2") {
                        plot.DBEST(DBEST.values, figure=2)
                }

        }
        
        
        
        return(DBEST.values)
        
}
