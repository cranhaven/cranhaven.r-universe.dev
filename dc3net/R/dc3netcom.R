dc3netcom <- function(netTemp, vals, rankdif, percent)
{
  
  # netTemp is test net list, e.g. tumor network.
  # vals variable is statistics in the other (control) condition
  
  numlinks <- nrow(netTemp)
  
  AdjL <- c()
  
  for(i in 1: numlinks){  
    
    if(  (as.numeric(vals[i,4]) < rankdif & as.numeric(vals[i,1]) > (as.numeric(vals[i,2])*percent))  |  (as.numeric(vals[i,5]) < rankdif & as.numeric(vals[i,1]) > (as.numeric(vals[i,3])*percent)  ))  
    {
      
      temp <- append(netTemp[i,],i) 
      
      temp <- append(temp,(as.numeric(vals[i,1])/as.numeric(vals[i,2])))
      
      temp <- append(temp, as.numeric(vals[i,4]))     
      
      temp <- append(temp,(as.numeric(vals[i,1])/as.numeric(vals[i,3])))
      
      temp <- append(temp, as.numeric(vals[i,5]))
      
      AdjL <- rbind(AdjL,temp)		
    }
    
  }
  
  ix <- ncol(AdjL)	
  if(!is.null(AdjL)) {	
    colnames(AdjL)[(ix-4)] <- "ContIndx"
    colnames(AdjL)[(ix-3)] <- "ContrlMIrateG1"
    colnames(AdjL)[(ix-2)] <- "ContrlRankG1"
    colnames(AdjL)[(ix-1)] <- "ContrlMIrateG2"
    colnames(AdjL)[ix] <- "ContrlRankG2"
    
    rownames(AdjL) <- c(1:nrow(AdjL))

  }else cat("DC3NET: No differential interactions found. Try less stringent cutoffs.\n")
  
  return(AdjL)
}
