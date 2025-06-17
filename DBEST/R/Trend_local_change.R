Trend_local_change <-
function(data_series,epsilon,vec) {
        
        Length <- length(data_series); 
        n_turning <- 0 
        Distance_points <- 999
        Turning <- c()
        f_sign <- c()
        f_local <- c()
        
        if(epsilon<=0) {
                n_turning <- Length-1
                f_sign <- rep(1,Length-1)
                f_sign[1] <- 0  
                Turning <- seq(Length-1)
                RL_DIST <- rep(0,(Length-1))
        } else {
                delta1 <- 0
                delta2 <- 0
                ck <- 2
                for(k in 2:(Length-1)) {
                        
                        delta1[ck]=data_series[ck]-data_series[ck -1]
                        delta2[ck]=data_series[ck + 1]-data_series[ck]
                        
                        if( sign(delta1[ck]) != sign(delta2[ck]) ) {
                                f_sign[ck] <- 1
                                n_turning <- n_turning + 1
                                Turning[n_turning] <- ck
                        } else {
                                f_sign[ck] <- 0
                        }
                        ck <- ck + 1
                }
                
                f_sign[1] <- 1
                n_turning <- n_turning + 1
                f_sign[Length] <- 0
                
                while(length(Distance_points) != 0) {
                        
                        vertexs <- c(1,Turning,Length)
                        
                        rigth_dist1 <- c()
                        left_dist1 <- c()
                        rigth_dist2 <- c()
                        left_dist2 <- c()

                        Rigth_Dist <- c()
                        Left_Dist <- c()
                        rigth_dist_max <- 0
                        left_dist_max <- 0
                        
                        for(i in 1:n_turning) {
                                
                                v1_x <- vertexs[i]
                                v2_x <- vertexs[i+1]
                                v1_y <- data_series[v1_x]
                                v2_y <- data_series[v2_x]
                                points_within <- v2_x - v1_x

                                
                                if(points_within>1) {
                                        line_slope <- (v2_y-v1_y)/points_within
                                
                                        for(j in 1:(points_within-1)){
                                                
                                                point_slope <- (data_series[v1_x+j]-v1_y)/j
                                                if(point_slope<=line_slope) {
                                                        rigth_dist1[j] <- v1_x+j
                                                        rigth_dist2[j] <- Point_to_line( c(v1_x+j,data_series[v1_x+j],0), c(v1_x,v1_y,0), c(v2_x,v2_y,0) )
                                                        rigth_dist1[is.na(rigth_dist1)] <- 0
                                                        rigth_dist2[is.na(rigth_dist2)] <- 0
                                                } else {
                                                        left_dist1[j] <- v1_x+j
                                                        left_dist2[j] <- Point_to_line(c(v1_x+j,data_series[v1_x+j],0), c(v1_x,v1_y,0), c(v2_x,v2_y,0))
                                                        left_dist1[is.na(left_dist1)] <- 0
                                                        left_dist2[is.na(left_dist2)] <- 0
                                                }
                                                      
                                        } ##end for

                                        if( length(rigth_dist1) > 0 ) {
                                                rigth_mx_idx <- which(rigth_dist2==max(rigth_dist2))
                                                rigth_dist_max <- cbind(rigth_dist1[rigth_mx_idx], rigth_dist2[rigth_mx_idx])
                                                rigth_dist1 <- c()
                                                rigth_dist2 <- c()
                                                Rigth_Dist <- rbind(Rigth_Dist,rigth_dist_max)
                                        }
                                        
                                        if( length(left_dist1) > 0 ) {
                                                left_mx_idx <- which(left_dist2==max(left_dist2))
                                                left_dist_max <- cbind(left_dist1[left_mx_idx], left_dist2[left_mx_idx])
                                                left_dist1 <- c()
                                                left_dist2 <- c()
                                                Left_Dist <- rbind(Left_Dist,left_dist_max)
                                        } 
                                        
                                        
                                        
                                } ## end points_within  

                        } ## end for
                        
                        RL_Dist <- rbind(Rigth_Dist,Left_Dist)
                        Distance_points <- NULL
                        
                        if(length(RL_Dist) != 0) {
                          
                                RL_DIST <- RL_Dist[,2] 
                                
                                size_RL_Dist <- dim(RL_Dist)
                                
                                for(i in 1:size_RL_Dist[1]) {
                                  if(RL_Dist[i,2]>epsilon) {
                                    Distance_points <- c(Distance_points,RL_Dist[i,1])
                                  }
                                }
                                
                        } else {
                                RL_DIST <- c()
                        }
                        

                        
                        n_turning <- n_turning + length(Distance_points)
                        f_sign[Distance_points] <- 1
                        Turning <- sort(c(Turning,Distance_points))  
                        
                } ## end while

                if(length(vec)>0) {
                        for(i in 1:length(vec)){
                                if(f_sign[vec[i]]!=1){
                                        f_sign[vec[i]] <- 1
                                        n_turning <- n_turning+1
                                        Turning <- c(Turning,vec[i])
                                }   
                        }
                }

                
                Turning <- sort(c(1,Turning))
        } ## end else

        f_diff <- rep(0,Length)
        
        for(i in 1:n_turning-1) {
                M_current <- data_series[Turning[i]]   
                M_after <- data_series[Turning[i+1]]
                f_diff[Turning[i]] <- M_after - M_current
        }
        
        f_diff[Turning[n_turning]] <- data_series[length(data_series)] - data_series[Turning[n_turning]]
        
        for(k in 1:(Length-1)) {
               
               f_local[k] <- f_sign[k] * f_diff[k]
        }
        f_local[Length] <- 0
        
        TLC.values <- list(
                "n_turning" = n_turning,
                "f_local" = f_local,
                "f_sign" = f_sign,
                "Turning" = Turning,
                "RL_DIST" = RL_DIST
        )
        
        class(TLC.values) <- "TLC"
        
        return(TLC.values)

}
