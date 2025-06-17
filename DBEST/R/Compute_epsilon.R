Compute_epsilon <-
function(pixeldata,epsilon_max,vec) {

        
        TLC <- Trend_local_change(pixeldata,epsilon_max,vec)
        
        turning_no_max <- TLC$n_turning
        f_local_max <- TLC$f_local
        f_sign_max <- TLC$f_sign
        Turning_max <- TLC$Turning
        RL_DISTmax <- TLC$RL_DIST
        DTSBP <- DBEST_timeseries_BP(pixeldata,turning_no_max,f_local_max,turning_no_max,f_sign_max,Turning_max)
        
        RSS <- DTSBP$RSS
        RMSE <- DTSBP$RMSE
        MAD <- DTSBP$MAD
        
        RMSE_max <- RMSE
        epsilon <- 3*(RMSE_max)
        return(epsilon)
        
}
