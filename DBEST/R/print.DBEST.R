print.DBEST <-
function(x, ...) { 
        
        cat("\nDBEST Results:\n")
        
        if(exists("RMSE", where=x)){
                
                cat("Number of segments: ", x$SegmentNo,"\n")
                cat("RMSE: ",x$RMSE,"\n")
                cat("MAD: ",x$MAD,"\n")
                cat("Fit: ",x$Fit,"\n")
                
        } else {
                cat("Number of break points: ", x$BreakpointNo,"\n")
                cat("Number of segments: ",x$SegmentNo,"\n")
                cat("Starting at: ",x$Start,"\n")
                cat("Duration: ",x$Duration,"\n")
                cat("Ending at: ",x$End,"\n")
                cat("Change: ",x$Change,"\n")
                cat("Change type: ",x$ChangeType,"\n")
                cat("Significance: ",x$Significance,"\n")
                cat("Fit: ",x$Fit,"\n")
                
        }
        
        
        invisible(x)
        
        
}
