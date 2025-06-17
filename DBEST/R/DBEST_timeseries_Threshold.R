DBEST_timeseries_Threshold <-
function(data_series,beta,f_local,n_turning,f_sign,Turning,BIC_no){
        
        Length <- length(data_series)
        f_local_abs <- abs(f_local)
        ith_max <- c()
        BP_points <- c()
        B1 <- c()
        B2 <- c()
        B3 <- c()
        B4 <- c()
        detrend_data_series <- c()
        f_sorted <- c()
        Index <- c()
        Start <- c()
        End <- c()
        Duration <- c()
        Magnitude <- c()
        BP_order <- c()
        BP_mark <- c()
        
        Tempnew <- f_local_abs[Turning]
        
        f_sorted <- Tempnew[order(-Tempnew)]
        Index <- order(-Tempnew)
        Index <- Turning[Index]
        
        n_BP <- length(f_local_abs[which(f_local_abs>beta)])
        
        if(n_BP>BIC_no){
                n_BP <- BIC_no
        }
        

        ith_max <- matrix(0,Length,n_BP)
        
        if(n_BP > 0) {
                for(i in 1:n_BP) {
                        ith_max[which(f_local_abs==f_sorted[i]),i] <- 1
                }
        }
        
        
	#BP_points <- rep(0,length(data_series))
	BP_points <- rep_len(0,length(data_series))

        
        if(n_BP > 0) {
                BP_points[Index[1:n_BP]] <- 1
        }
        
        if(n_BP==n_turning) {
                
                DETREND_DS <- DETREND_SJ(data_series,'linear',which(BP_points==1))
                detrend_data_series <- DETREND_DS$y
                param_no <- DETREND_DS$param_no
                
        } else {
                
                B1 <- which(BP_points==1)
                
                if(n_BP > 0) {
                        for(i in 1:n_BP){
                                B2 <- which(f_sign==1)
                                B3 <- B2[B2>B1[i]]
                                if(length(B3)==0) {
                                        B4 <- B4
                                } else {
                                        B4 <- c(B4,B3[1])
                                }
                        }
                        
                        DETREND_DS <- DETREND_SJ(data_series,'linear',c(which(BP_points==1),B4))
                } else {
                        
                        DETREND_DS <- DETREND_SJ(data_series,'linear')
                }

                detrend_data_series <- DETREND_DS$y
                param_no <- DETREND_DS$param_no
        }
        temp <- sort(c(which(BP_points==1), B4))
        b <- unique(temp)
        m1 <-  match(b,temp)
        
        c1 <- sort(m1)
        d1 <- match(c1,m1)
        b <- b[d1]
        shared <-b
        
        RSS <- sum(detrend_data_series^2)
        trend_data_series <- data_series - detrend_data_series
        
        #
        if(n_BP>0){
                Start<-which(BP_points==1)
                
                for(i in 1:n_BP) {
                        if(Turning[length(Turning)] == Start[i]) {
                                End[i] <- Length
                        } else {
                                End[i]= Turning[which(Turning==Start[i])+1]
                        }
                } # end for
                
                Duration <- End-Start
                Magnitude <- trend_data_series[End]-trend_data_series[Start] 
                
                absMagnitude <- abs(Magnitude)
                af <-  absMagnitude[order(-absMagnitude)] 
                bf <- order(-absMagnitude)
                
                Start <- Start[bf]
                End <- End[bf]
                Magnitude <- Magnitude[bf]
                Duration <- Duration[bf]
        }
        
        BP_mark <- matrix(0,Length,n_BP)
        
        if(n_BP > 0) {
                for(i in 1:n_BP) {
                        BP_mark[,i] <- data_series * ith_max[,i]
                }
        }

        BP_order <- BP_mark
        BP_mark[BP_mark==0] <- NaN
        
        RMSE <- sum(abs(detrend_data_series)^2)^(1/2) / sqrt(length(detrend_data_series))
        MAD <- max(abs(detrend_data_series))
        
        DTSTH.values <- list(
                "n_BP" = n_BP,
                "Start" = Start,
                "End" = End,
                "Duration" = Duration,
                "Magnitude" = Magnitude,
                "BP_mark" = BP_mark,
                "BP_order" = BP_order,
                "trend_data_series" = trend_data_series,
                "Param_no" = param_no,
                "RSS" = RSS,
                "RMSE" = RMSE,
                "MAD" = MAD,
                "shared" = shared
        )
        
        class(DTSTH.values) <- "DTSTH"
        
        return(DTSTH.values)
        
}
