f_level_shift <-
function(data_series,threshold1,threshold2,duration) {

        
        diff1 <- data_series[-1] - data_series[-length(data_series)]
        Index <- which(abs(diff1)>threshold1)
        diff2 <- rep(0, length(Index)) 
        
        number_temp <- 0
        number <- 0
        LS_candidate <- c()
        LS_mag <- c()
        LS_point <- c()
        
        if(length(Index) > 0) {
                
                for(i in 1:length(Index)) {
                        
                        Up <- Index[i] + duration
                        Down <- Index[i] - duration
                        
                        if ( (Up > length(data_series)) || (Down < 1) ) {
                                diff2[i] <- 0
                        } else {
                                diff2[i] <- mean( data_series[Index[i]:Up]  ) - mean( data_series[Down:Index[i]] )
                        }
                        
                        if( (abs(diff2[i]) > threshold2) && (sign(diff1[Index[i]]) == sign(diff2[i]))  ) {
                                
                                number_temp <- number_temp + 1
                                LS_candidate[number_temp] <- Index[i]
                                LS_mag[number_temp] <- diff2[i]
                                
                        }
                        
                }
                
        }


        if(number_temp > 0) {
                
                absLS_mag <- abs(LS_mag)
                LS_mag_sorted <-  absLS_mag[order(-absLS_mag)] #descend
                index_sort <- order(-absLS_mag)
                number <- 1
                LS_point[number] <- LS_candidate[index_sort[1]]
                
                if(number_temp>1) {
                        ci <- 2
                        for(i in 2:number_temp) {
                                t <- 0
                                for(j in 1:number) {
                                        if( abs(LS_candidate[index_sort[ci]] - LS_point[j]) > duration ) {
                                                t <- t + 1
                                        }
                                }
                                if (t == number) {
                                        number <- number + 1
                                        LS_point[number] <- LS_candidate[index_sort[ci]]
                                }
                                ci <- ci + 1
                                
                        }
                        
                }

                
        }
        
        flevel.values <- list(
                "no" = number,                
                "LS_point" = LS_point
        )
        
        class(flevel.values) <- "flevel"
        return(flevel.values)        
}
